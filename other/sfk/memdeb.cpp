/*
   Swiss File Knife simple memory tracing.
   Unlimited Open Source, free for use in any project.

   This memdeb.cpp must be included by every .cpp source file
   that needs memory tracing. All source files except one
   must define MEMDEB_JUST_DECLARE.

   1.0.2
   -  complete rework and cleanup, for better suitability
      across multiple sources.
   -  VERBOSE_MEM now using mtklog instead of printf.

   1.0.1
   -  massive performance improvement by removal of
      separate memory block list. the control blocks
      are now stored directly in front of user memory.
*/

#ifdef VERBOSE_MEM
 #include "mtk/mtktrace.hpp"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

class SFKMemoryListEntry
{
public:
   SFKMemoryListEntry   ( ) { pClNext = pClPrevious = 0; }

   SFKMemoryListEntry *next      ( ) { return pClNext; }
   SFKMemoryListEntry *previous  ( ) { return pClPrevious; }

   SFKMemoryListEntry *pClNext;
   SFKMemoryListEntry *pClPrevious;
};

class SFKMemoryList
{
public:
   SFKMemoryList  ( )   { pClFirst = pClLast = 0; }

   SFKMemoryListEntry *first  ( ) { return pClFirst; }
   SFKMemoryListEntry *last   ( ) { return pClLast; }

   void add                (SFKMemoryListEntry *);
   void addAsFirst         (SFKMemoryListEntry *);
   void addAfter           (SFKMemoryListEntry *after, SFKMemoryListEntry *toadd);
   void remove             (SFKMemoryListEntry *entry);  // w/o delete
   void removeAndDeleteAll ( );

private:
   SFKMemoryListEntry *pClFirst;
   SFKMemoryListEntry *pClLast;
};

struct SFKMemoryBlock
 : public SFKMemoryListEntry
{
long   lSize;
long   lLine;
char  *file;
long   bValid;
char   abRedZone[4];
};

// new and delete op's MUST be declared in EVERY .cpp source,
// otherwise redirection cannot be assured.
static void *operator new[](size_t size, char *src, int line);
static void *operator new(size_t size, char *src, int line);

#ifdef MEMDEB_JUST_DECLARE

extern void  sfkmem_hexdump(void *pAddressIn, long  lSize);
extern void *sfkmem_debnew(size_t size, char *pszSource, int line);
extern void  sfkmem_debdel(void *pUserMemory);
extern long  anyMemoryLeaks();
extern long  listMemoryLeaks(FILE *fout=0);
extern char *sfkmem_strdup(const char *strSource, char *pszFile, int nLine);
extern void  sfkmem_setZone(void *p,size_t size);
extern long  sfkmem_checkZone(void *p,size_t size);
extern void  sfkmem_setpredel(char *file, long line);
extern void  sfkmem_nocheck();

#else

SFKMemoryList glblMem;

char *sfkmem_file = 0;
long  sfkmem_line = 0;
long  sfkmem_news = 0;
long  sfkmem_dels = 0;
long  sfkmem_errs = 0;
size_t sfkmem_bytes = 0;
char  sfkmem_bnocheck = 0;

void SFKMemoryList :: add(SFKMemoryListEntry* pNew)
{
   if (!pClFirst)
   {
      // yet empty list:
      pClFirst = pClLast = pNew;
      pNew->pClNext = pNew->pClPrevious = 0;
      return;
   }

   // append node at end of list:
   pClLast->pClNext   = pNew;
    pNew->pClPrevious = pClLast;
    pNew->pClNext     = 0;
   pClLast            = pNew;
}

void SFKMemoryList :: addAsFirst(SFKMemoryListEntry* pNew)
{
   // yet empty list?
   if (!pClFirst)
   {
      pClFirst = pClLast = pNew;
      pNew->pClNext = pNew->pClPrevious = 0;
      return;
   }

   // make node new front of list:
   SFKMemoryListEntry *n2 = pClFirst;
   n2->pClPrevious      = pNew;
    pNew->pClPrevious   = 0;
    pNew->pClNext       = n2;
   pClFirst             = pNew;
}

void SFKMemoryList :: addAfter(SFKMemoryListEntry *pAfter, SFKMemoryListEntry *pNew)
{
   SFKMemoryListEntry *pNext = pAfter->pClNext;  // might be 0

   pAfter->pClNext   = pNew;
   pNew->pClPrevious = pAfter;
   pNew->pClNext     = pNext;

   if (pNext)
      pNext->pClPrevious = pNew;
   else
      pClLast  = pNew;
}

void SFKMemoryList :: remove(SFKMemoryListEntry* pRemove)
{
   SFKMemoryListEntry *pPrevious = pRemove->pClPrevious;  // might be 0
   SFKMemoryListEntry *pNext     = pRemove->pClNext;      // might be 0

   pRemove->pClNext = pRemove->pClPrevious = 0;

   if (!pPrevious)   // if 'pRemove' at start of list
   {
      if (pClFirst = pNext)      // new list start becomes pNext ...
         pNext->pClPrevious = 0; // ... and if pNext exists, adjust it,
      else
         pClLast  = 0;           // else list is empty.
   }
   else
   {
      // at least a 'pClPrevious' is given.
      if (pPrevious->pClNext = pNext)     // let pPrevious' 'pClNext' ptr bypass 'pRemove' ...
         pNext->pClPrevious = pPrevious;  // ... and if pNext exists, adjust it,
      else
         pClLast  = pPrevious;            // else set new listend.
   }
}

void sfkmem_hexdump(void *pAddressIn, long  lSize)
{
 char szBuf[100];
 long lIndent = 1;
 long lOutLen, lIndex, lIndex2, lOutLen2;
 long lRelPos;
 struct { char *pData; unsigned long lSize; } buf;
 unsigned char *pTmp,ucTmp;
 unsigned char *pAddress = (unsigned char *)pAddressIn;

   buf.pData   = (char *)pAddress;
   buf.lSize   = lSize;

   while (buf.lSize > 0)
   {
      pTmp     = (unsigned char *)buf.pData;
      lOutLen  = (int)buf.lSize;
      if (lOutLen > 16)
          lOutLen = 16;

      /*                        1         2         3         4          */
      /*              01234567890123456789012345678901234567890123456789 */
      sprintf(szBuf, " >                                                  "
                     "    %08lX", pTmp-pAddress);
      lOutLen2 = lOutLen;

      for(lIndex = 1+lIndent, lIndex2 = 53-15+lIndent, lRelPos = 0;
          lOutLen2;
          lOutLen2--, lIndex += 2, lIndex2++
         )
      {
         ucTmp = *pTmp++;

         sprintf(szBuf + lIndex, "%02X ", (unsigned short)ucTmp);
         if(!isprint(ucTmp))  ucTmp = '.'; // nonprintable char
         szBuf[lIndex2] = ucTmp;

         if (!(++lRelPos & 3))     // extra blank after 4 bytes
         {  lIndex++; szBuf[lIndex+2] = ' '; }
      }

      if (!(lRelPos & 3)) lIndex--;

      szBuf[lIndex  ]   = '<';
      szBuf[lIndex+1]   = ' ';

      printf("%s\n", szBuf);

      buf.pData   += lOutLen;
      buf.lSize   -= lOutLen;
   }
}

void sfkmem_nocheck()
{
   sfkmem_bnocheck = 1;
}

void sfkmem_setZone(void *p,size_t size)
{
   unsigned char *puc = (unsigned char *)p;   
   for (unsigned long i=0; i<size; i++)
      puc[i] = 0xEE;
}

long sfkmem_checkZone(void *p,size_t size)
{
   unsigned char *puc = (unsigned char *)p;   
   for (unsigned long i=0; i<size; i++)
      if (puc[i] != (unsigned char)0xEE)
         return -1;
   return 0;         
}

void *sfkmem_debnew(size_t size, char *pszSource, int line)
{
   long npre = sizeof(SFKMemoryBlock);

   // calculate red zone sizes
   long lrs1 = 4;
   long lrs2 = size / 10;
   if (lrs2 < 4) lrs2 = 4;

   char *ppre = (char*)malloc(npre + size + lrs2);
   // prezone size lrs1 is included in npre!
   
   if (!ppre) 
   {
      #ifdef VERBOSE_MEM
      mtklog(("%p = ALLOC %ld (%s, %ld)", ppre, size, pszSource, line));
      #endif

      return 0;
   }
   
   char *pUserMemory = ppre + npre;
   
   #ifdef VERBOSE_MEM
   mtklog(("%p = ALLOC %ld (%s, %ld) raw %p", pUserMemory, size, pszSource, line, ppre));
   #endif

   SFKMemoryBlock *pBlock = (SFKMemoryBlock *)ppre;
   pBlock->bValid   = 0x12345678;
   pBlock->lSize    = size;
   pBlock->lLine    = line;
   pBlock->file     = pszSource;

   if (sfkmem_bnocheck)
   {
      return (char*)pUserMemory;
   }

   sfkmem_setZone(pBlock->abRedZone,lrs1);
   sfkmem_setZone(pUserMemory+size ,lrs2);

   glblMem.addAsFirst(pBlock);

   sfkmem_news++;
   sfkmem_bytes += size;
   
   return (char*)pUserMemory;
}

void sfkmem_debdel(void *pUserMemory)
{   
   if (!pUserMemory)
      return;
  
   #ifdef VERBOSE_MEM
   mtklog(("%p   DELETE (%s, %ld)", pUserMemory, sfkmem_file, sfkmem_line));
   #endif

   long npre  = sizeof(SFKMemoryBlock);
   char *ppre = (char*)pUserMemory - npre;

   if (sfkmem_bnocheck)
   {
      free(ppre);
      return;
   }

   SFKMemoryBlock *p = (SFKMemoryBlock *)ppre;

   if (p->bValid == 0x12345678)
   {
      // calculate red zone sizes
      long lrs1 = 4;
      long lrs2 = p->lSize / 10;
      if (lrs2 < 4) lrs2 = 4;
 
      if (sfkmem_checkZone(p->abRedZone, 4))
      {
         printf("MEMORY OVERWRITE:\n"
            "  before block: %p\n"
            "  alloc'ed in : %s line %ld\n",
            pUserMemory,
            p->file, p->lLine);
            
         printf("hexdump of block start follows.\n"
            "first %ld (%lxh) bytes should be '0xEE',\n"
            "those not being 0xEE got hit by something.\n",
            lrs1, lrs1);
            
         long lMaxDump = p->lSize;
         if (lMaxDump > 100)
             lMaxDump = 100;
         sfkmem_hexdump(p->abRedZone, lrs1+lMaxDump);
         
         sfkmem_errs++;
      }
      
      if (sfkmem_checkZone((char*)pUserMemory+p->lSize,lrs2))
      {
         printf("MEMORY OVERWRITE:\n"
            "  after block: %p\n"
            "  alloc'ed in: %s line %ld\n",
            pUserMemory,
            p->file, p->lLine);
            
         printf("hexdump of block end follows.\n"
            "last %ld (%lxh) bytes should be '0xEE',\n"
            "those not being 0xEE got hit by something.\n",
            lrs2, lrs2);
            
         long lMaxDump = p->lSize;
         if (lMaxDump > 100)
             lMaxDump = 100;
         sfkmem_hexdump((char*)pUserMemory
            +p->lSize
            -lMaxDump,
            lMaxDump+lrs2);
            
         sfkmem_errs++;
      }

      sfkmem_bytes -= p->lSize;

      p->bValid = 0xEEEEEEEE;

      glblMem.remove(p);

      free(ppre);
   }
   else
   {
      printf("DELETE TWICE:\n"
         "  of block: %p\n"
         "  done in : %s line %ld\n",
         pUserMemory,
         sfkmem_file,
         sfkmem_line
         );
         
      sfkmem_errs++;
   }         
   
   sfkmem_dels++;  
}

long anyMemoryLeaks()
{
   if (sfkmem_bnocheck)
      return 0;

   return (glblMem.first() != 0) ? 1 : 0;
}

long listMemoryLeaks(FILE *fout=0)
{
   if (!fout) fout = stdout;

   long bAnyLeak = 0;

   // deep mode
   for (SFKMemoryBlock *p=(SFKMemoryBlock*)glblMem.first(); p; p=(SFKMemoryBlock*)p->next())
   {
      bAnyLeak = 1;
      char *pAddress = (char*)p + sizeof(SFKMemoryBlock);
      fprintf(fout, "MEM LEAK: adr %p, size %ld, alloc'ed in %s %ld\n",
         pAddress,
         p->lSize,
         p->file,
         p->lLine
         );
   }

   if (bAnyLeak)
      fprintf(fout, "[SMEMDEBUG: %ld new's, %ld delete's, %ld errors, %ld leaks]\n",
         sfkmem_news, sfkmem_dels, sfkmem_errs, sfkmem_news-sfkmem_dels);

   return bAnyLeak;
}

// exits with code 10 on any error
void sfkmem_checklist(const char *pszCheckPoint)
{
   int iBlock = 0;
   int iErrors = 0;

   for (SFKMemoryBlock *p=(SFKMemoryBlock*)glblMem.first(); p; p=(SFKMemoryBlock*)p->next())
   {
      iBlock++;   

      char *pUserMemory = ((char*)p) + sizeof(SFKMemoryBlock);

      if (p->bValid == 0x12345678)
      {
         // calculate red zone sizes
         long lrs1 = 4;
         long lrs2 = p->lSize / 10;
         if (lrs2 < 4) lrs2 = 4;
   
         if (sfkmem_checkZone(p->abRedZone, 4))
         {
            printf("MEMORY OVERWRITE after %s\n"
               "  before block: %p\n"
               "  alloc'ed in : %s line %ld\n",
               pszCheckPoint,
               pUserMemory,
               p->file, p->lLine);
   
            printf("hexdump of block start follows.\n"
               "first %ld (%lxh) bytes should be '0xEE',\n"
               "those not being 0xEE got hit by something.\n",
               lrs1, lrs1);
   
            long lMaxDump = p->lSize;
            if (lMaxDump > 100)
                lMaxDump = 100;
            sfkmem_hexdump(p->abRedZone, lrs1+lMaxDump);
   
            iErrors++;
         }
   
         if (sfkmem_checkZone((char*)pUserMemory+p->lSize,lrs2))
         {
            printf("MEMORY OVERWRITE after %s\n"
               "  after block: %p\n"
               "  alloc'ed in: %s line %ld\n",
               pszCheckPoint,
               pUserMemory,
               p->file, p->lLine);
   
            printf("hexdump of block end follows.\n"
               "last %ld (%lxh) bytes should be '0xEE',\n"
               "those not being 0xEE got hit by something.\n",
               lrs2, lrs2);
   
            long lMaxDump = p->lSize;
            if (lMaxDump > 100)
                lMaxDump = 100;
            sfkmem_hexdump((char*)pUserMemory
               +p->lSize
               -lMaxDump,
               lMaxDump+lrs2);
   
            iErrors++;
         }
      }
      else
      {
         printf("error: MEMORY LIST CORRUPTED after %s\n"
            "  in block: %p\n"
            "  number  : %d\n",
            pszCheckPoint,
            pUserMemory,
            iBlock
            );

         iErrors++;
         break; // MUST stop then
      }
   }

   if (iErrors > 0) 
   {
      printf("sfk exits with code 10 due to %d memory errors.\n", iErrors);
      exit(10);
   }
}

char *sfkmem_strdup(const char *strSource, char *pszFile, int nLine) 
{
   long lLen  = strlen(strSource);
   char *pOut = new(pszFile,nLine) char[lLen+2];
   strcpy(pOut, strSource);
   return pOut;
}

void sfkmem_setpredel(char *file, long line)
{
   sfkmem_file = file;
   sfkmem_line = line;
}

#endif // just_declare

static void *operator new(size_t size, char *pszSource, int line)
{  return sfkmem_debnew(size, pszSource, line);  }

static void *operator new[](size_t size, char *pszSource, int line)
{  return sfkmem_debnew(size, pszSource, line);  }

static void operator delete(void *pUserMemory)
{  sfkmem_debdel(pUserMemory);  }

#define newTmpPreProc50796 new(__FILE__,__LINE__)
#define new newTmpPreProc50796

#define delTmpPreProc50796 delete
#define delete sfkmem_setpredel(__FILE__,__LINE__),delTmpPreProc50796

#define strdup(x) sfkmem_strdup(x,__FILE__,__LINE__)
