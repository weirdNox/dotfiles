/*
   SFKMatch - the Swiss File Knife Simple Expression Parser V 1.0
   ==============================================================
*/

#ifndef SFKMATCH_IMPORTED

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#ifdef _WIN32
 #include <windows.h>
 #define vsnprintf _vsnprintf
 #define snprintf  _snprintf
 const char  glblPathChar = '\\';
#else
 const char  glblPathChar = '/';
#endif

#include "sfkmatch.hpp"

#define mymin(a,b) ((a<b)?(a):(b))

#ifdef SFKVAR
int sfksetvar(char *pname, uchar *pdata, int idata, int nadd=0);
uchar *sfkgetvar(char *pname, int *plen);
#endif

// print error to terminal
int sfkerr(const char *pszFormat, ...)
{
   char szErrBuf[200];

   va_list argList;
   va_start(argList, pszFormat);
   ::vsnprintf(szErrBuf, sizeof(szErrBuf)-10, pszFormat, argList);
   szErrBuf[sizeof(szErrBuf)-10] = '\0';

   if (!strchr(szErrBuf, '\n'))
      strcat(szErrBuf, "\n");

   fprintf(stderr, "error: %s", szErrBuf);

   return 0;
}

// print info to terminal
int sfkinf(const char *pszFormat, ...)
{
   char szErrBuf[200];

   va_list argList;
   va_start(argList, pszFormat);
   ::vsnprintf(szErrBuf, sizeof(szErrBuf)-10, pszFormat, argList);
   szErrBuf[sizeof(szErrBuf)-10] = '\0';

   if (!strchr(szErrBuf, '\n'))
      strcat(szErrBuf, "\n");

   fprintf(stdout, "%s", szErrBuf);

   return 0;
}

char *SFKMatch::dataAsTrace(void *pAnyData, int iDataSize, char *pszBuf, int iMaxBuf)
{
   static char szBuf[300];

   if (!pszBuf)
   {
      pszBuf = szBuf;
      iMaxBuf = sizeof(szBuf);
   }
 
   uint8_t *pSrcCur = (uint8_t *)pAnyData;
   uint8_t *pSrcMax = pSrcCur + iDataSize;
 
   char *pszDstCur = pszBuf;
   char *pszDstMax = pszBuf + iMaxBuf - 20;
 
   while (pSrcCur < pSrcMax && pszDstCur < pszDstMax)
   {
      uint8_t uc = *pSrcCur++;
 
      if (isprint((char)uc))
      {
         *pszDstCur++ = (char)uc;
         continue;
      }

      // convert binary to {hex}
      sprintf(pszDstCur, "{%02X}", uc);
      pszDstCur += 4;
   }
 
   *pszDstCur = '\0';
 
   return pszBuf;
}

class NoCaseText
{
public:
   NoCaseText    ( );
   void reinit (bool bISO);

   inline  char lowerChar ( char c) { return aClLowerTab[(uint8_t)c]; }
   inline uint8_t lowerUChar(uint8_t c) { return (uint8_t)aClLowerTab[(uint8_t)c]; }
   inline uint8_t mapChar   (uint8_t c, uint8_t bCase) { return bCase ? c : (uint8_t)aClLowerTab[(uint8_t)c]; }
   void  setStringToLower(char *psz);

   char aClLowerTab[256+10];
};

NoCaseText glblNoCase; // module test only

NoCaseText::NoCaseText()
{
   reinit(1); // default
}

void NoCaseText::reinit(bool bISO)
{
   memset(aClLowerTab, 0, sizeof(aClLowerTab));

   for (uint32_t u1=0; u1<256; u1++)
   {
      uint8_t u2 = (uint8_t)u1;

      if (u1 >= 0x41 && u1 <= 0x5A)    // A-Z
         u2 += 0x20U;    // -> a-z

      if (bISO)
      {
         // ISO 8859-1 special character lowercase mapping
         if (u1 >= 0xC0 && u1 <= 0xDE)    // special characters
            if (u1 != 0xD7 && u1 != 0xDF) // NOT these two
               u2 += 0x20U; // e.g. Ä -> ä
      }

      aClLowerTab[u1] = (char)u2;
   }
}

void NoCaseText::setStringToLower(char *psz)
{
   for (int i=0; psz[i]; i++)
   {
      psz[i] = aClLowerTab[(uint8_t)psz[i]];
   }
}

inline void sfkSetBit(uint8_t *pField, uint32_t iBit)
{
   pField[iBit>>3] |= (1U << (iBit & 7));
}

inline uint8_t sfkGetBit(uint8_t *pField, uint32_t iBit)
{
   return (pField[iBit>>3] & (1U << (iBit & 7))) ? 1 : 0;
}

// RC : 0 == match, <> 0 == no match.
// Not suitable for sorting algorithms.
int sfkmemcmp2(uint8_t *psrc1, uint8_t *psrc2, int64_t nlen, bool bGlobalCase, uint8_t *pFlags)
{
   if (bGlobalCase)
      return memcmp(psrc1, psrc2, nlen);

   int idiff=0;

   // optim: compare last character first.
   // requires at least a 2-char phrase.
   if (nlen > 1)
   {
      uint8_t bCase = pFlags ? sfkGetBit(pFlags,nlen-1) : 0;
      idiff =     glblNoCase.mapChar(psrc1[nlen-1],bCase)
               -  glblNoCase.mapChar(psrc2[nlen-1],bCase);
      if (idiff)
         return idiff;
   }

   uint8_t bCase;

   for (int i=0; i<nlen; i++)
   {
      bCase = pFlags ? sfkGetBit(pFlags,i) : 0;
      idiff =     glblNoCase.mapChar(psrc1[i],bCase)
               -  glblNoCase.mapChar(psrc2[i],bCase);
      if (idiff)
         break;
   }

   return idiff;
}

// RC : 0 == match, <> 0 == no match.
// Not suitable for sorting algorithms.
int sfkmemcmp3(uint8_t *psrc, uint8_t *ppat, int64_t nlen, bool bGlobalCase, uint8_t *pPatFlags, int iPatOff)
{
   ppat += iPatOff;

   if (bGlobalCase)
      return memcmp(psrc, ppat, nlen);

   int idiff=0;

   // optim: compare last character first.
   // requires at least a 2-char phrase.
   if (nlen > 1)
   {
      uint8_t bCase = pPatFlags ? sfkGetBit(pPatFlags,nlen+iPatOff-1) : 0;
      idiff =     glblNoCase.mapChar(psrc[nlen-1],bCase)
               -  glblNoCase.mapChar(ppat[nlen-1],bCase);
      if (idiff)
         return idiff;
   }

   uint8_t bCase;

   for (int i=0; i<nlen; i++)
   {
      bCase = pPatFlags ? sfkGetBit(pPatFlags,iPatOff+i) : 0;
      idiff =     glblNoCase.mapChar(psrc[i],bCase)
               -  glblNoCase.mapChar(ppat[i],bCase);
      if (idiff)
         break;
   }

   return idiff;
}

// RC : 0 == match, <> 0 == no match.
// Not suitable for sorting algorithms.
int sfkmemcmp(uint8_t *psrc1, uint8_t *psrc2, int64_t nlen, bool bcase)
{
   if (bcase)
      return memcmp(psrc1, psrc2, nlen);

   int64_t i=0, idiff=0;

   // optim: compare last character first.
   // requires at least a 2-char phrase.
   if (nlen > 1)
   {
      idiff =     sfkLowerUChar(psrc1[nlen-1])
               -  sfkLowerUChar(psrc2[nlen-1]);
      if (idiff)
         return idiff;
   }

   for (; i<nlen; i++)
   {
      idiff =     sfkLowerUChar(psrc1[i])
               -  sfkLowerUChar(psrc2[i]);

      if (idiff)
         break;
   }

   return idiff;
}

bool myisxdigit(char c) {
   if (c >= '0' && c <= '9') return 1;
   if (c >= 'a' && c <= 'f') return 1;
   if (c >= 'A' && c <= 'F') return 1;
   return 0;
}

int getTwoDigitHex(char *psz)
{
   char szHex[10];

   if (!*psz) return -1;
   szHex[0] = tolower(*psz++);
   if (!myisxdigit(szHex[0])) return -1;

   if (!*psz) return -1;
   szHex[1] = tolower(*psz++);
   if (!myisxdigit(szHex[1])) return -1;

   szHex[2] = '\0';

   return (int)strtoul(szHex,0,0x10);
}

int getThreeDigitDec(char *psz)
{
   char szDec[10];

   if (!*psz) return -1;
   szDec[0] = tolower(*psz++);
   if (!isdigit(szDec[0])) return -1;

   if (!*psz) return -1;
   szDec[1] = tolower(*psz++);
   if (!isdigit(szDec[1])) return -1;

   if (!*psz) return -1;
   szDec[2] = tolower(*psz++);
   if (!isdigit(szDec[2])) return -1;

   szDec[3] = '\0';

   return (int)strtoul(szDec,0,10);
}

#else

#define sfkerr perr
#define sfkinf pinf

#endif // SFKMATCH_IMPORTED

enum SFKMatchTokens
{
   TokLiteral  =  1,
   TokClass    =  2,
   TokByte     =  3,
   TokStart    =  4,
   TokStartOr  =  5,
   TokLStart   =  6,
   TokEnd      =  7,
   TokEOL      =  8,
   TokEndOrEOL =  9,
   TokPart     = 10,
   TokOutFN    = 11,
   #ifdef SFKVAR
   TokSetVar   = 12,
   TokEndVar   = 13,
   TokGetVar   = 14,
   #endif
};

enum SFKMatchExpandPrios
{
   XPrioAnchor    = 5,
   XPrioLiteral   = 4,
   XPrioWhiteList = 3,
   XPrioBlackList = 2,
   XPrioWildCard  = 1
};

// bits 0..3 are token prio
#define TOKOPT_PRIOMASK 15
#define TOKOPT_REFLEN   16
#define TOKOPT_OR       32
#define TOKOPT_LEAN     64
#define TOKOPT_KEEP     128   // [keep]

int SFKMatch::iClOff = 4; // must be >= 1
int SFKMatch::iClOutSizeMax = 32768; // default
int SFKMatch::iClOutSizeAlloc = 0;
int SFKMatch::iClExpBuf = 0;
int SFKMatch::iClDefUsed = 0;
int SFKMatch::iClDefAlloc = 0;
uint8_t *SFKMatch::aClOutBuf = 0;
uint8_t *SFKMatch::aClExpBuf = 0;
char  *SFKMatch::aClOutAttr = 0;
char  **SFKMatch::aClDefFrom = 0;
char  **SFKMatch::aClDefTo = 0;
bool SFKMatch::bClUseCase = 0;
bool SFKMatch::bClXChars = 0;
bool SFKMatch::bClExtract = 0;
bool SFKMatch::bClXText = 0;
char SFKMatch::cClLitAttr = '\0';
int SFKMatch::iClTrace = 0;

uint8_t SFKMatch::aClLitBuf[SFKMATCH_MAX_LITERAL_SIZE+100];
uint8_t SFKMatch::aClLitBufBit[(SFKMATCH_MAX_LITERAL_SIZE+100)/8];
uint8_t SFKMatch::bClStaticInitDone = 0;
uint8_t SFKMatch::aClSkipMatchChars[256];
uint8_t SFKMatch::aClSkipMatchBytes[256];
uint8_t SFKMatch::aClSkipMatchWhite[256];
uint8_t SFKMatch::aClSkipMatchXWhite[256];
uint8_t SFKMatch::aClSkipMatchAlpha[256];
uint8_t SFKMatch::aClSkipMatchAlnum[256];
uint8_t SFKMatch::aClSkipMatchDigits[256];
uint8_t SFKMatch::aClSkipMatchHexDig[256];
uint8_t SFKMatch::aClFromCopy[512];
uint8_t SFKMatch::aClPartInfo[512];
uint8_t SFKMatch::aClPartInfo2[512];
uint8_t SFKMatch::aClPrioInfo2[512];

SFKMatchOutFNCallback_t SFKMatch::pClOutFN = 0;

int SFKMatchDefaultMaxLen = SFKMATCH_DEFAULT_MAXLEN;
int SFKMatchByteWildCards = 0;

SFKMatch::SFKMatch( )
{
   memset(this, 0, sizeof(*this));
}

SFKMatch::~SFKMatch( )
{
   reset();
}

void SFKMatch::setGlobalOption(enum ESFKMatchOptions e, int iValue)
{
   switch (e)
   {
      case SFKMatchUseCase : bClUseCase = (bool)iValue; break;
      case SFKMatchXChars  : bClXChars  = (bool)iValue; break;
      case SFKMatchExtract : bClExtract = (bool)iValue; break;
      case SFKMatchLitAttr : cClLitAttr = (char)iValue; break;
      case SFKMatchTrace   : iClTrace   =  (int)iValue; break;
      case SFKMatchXText   : bClXText   = (bool)iValue; break;
   }
}

int SFKMatch::setOutFNCallback(SFKMatchOutFNCallback_t pFN)
{
   pClOutFN = pFN;
   return 0;
}

int SFKMatch::traceLevel( )
{
   return iClTrace;
}

int SFKMatch::staticMemory( )
{
   int iBytes = 0;
 
   iBytes += sizeof(aClLitBuf);
   iBytes += iClOutSizeMax; // aClOutBuf
   iBytes += iClOutSizeMax; // aClOutAttr

   return iBytes;
}

void SFKMatch::reset( )
{
   for (int i=0; i<iClFromTok; i++)
   {
      if (aClData && aClData[i])
         delete [] aClData[i];
      if (aClAttr && aClAttr[i])
         delete [] aClAttr[i];
      if (aClInFN && aClInFN[i])
         delete [] aClInFN[i];
      if (aClFlags && aClFlags[i])
         delete [] aClFlags[i];
      if (aClClass && aClClass[i] && aClDynaClass[i])
         delete [] aClClass[i];
      #ifdef SFK_LOR
      if (aClOrInf && aClOrInf[i])
         delete [] aClOrInf[i];
      #endif // SFK_LOR
   }
   for (int i=0; i<iClToTok; i++)
   {
      if (aClToLit && aClToLit[i])
         delete [] aClToLit[i];
   }

   if (aClFrom) delete [] aClFrom;
   if (aClData) delete [] aClData;
   if (aClAttr) delete [] aClAttr;
   if (aClInFN) delete [] aClInFN;
   if (aClFlags) delete [] aClFlags;
   if (aClTo) delete [] aClTo;
   if (aClToLit) delete [] aClToLit;
   if (aClClass) delete [] aClClass;
   if (aiClData) delete [] aiClData;
   if (aiClBestData) delete [] aiClBestData;
   if (aiClFromMinLen) delete [] aiClFromMinLen;
   if (aiClFromMaxLen) delete [] aiClFromMaxLen;
   if (aClTokOpts) delete [] aClTokOpts;
   if (aClOutOpts) delete [] aClOutOpts;
   if (aClDynaClass) delete [] aClDynaClass;
   #ifdef SFK_LOR
   if (aClOrInf) delete [] aClOrInf;
   #endif // SFK_LOR

   if (bClFromTextWasCopied) {
      if (pszClFromText) delete [] pszClFromText;
   }

   memset(this, 0, sizeof(*this));
}

int SFKMatch::objectFromRange( ) { return iClFromRange; }
int SFKMatch::objectToRange( )   { return iClToRange;   }

int SFKMatch::objectMemory( )
{
   int iBytes = 0;

   for (int i=0; i<iClFromTok; i++)
   {
      if (aClData && aClData[i]) {
         iBytes += aiClFromMaxLen[i];
         // printf("size: from #%u = %d\n",i,aClFrom[i] >> 8);
      }
      if (aClAttr && aClAttr[i]) {
         iBytes += aiClFromMaxLen[i];
         // printf("size: from #%u = %d\n",i,aClFrom[i] >> 8);
      }
      if (aClInFN && aClInFN[i]) {
         iBytes += strlen(aClInFN[i]);
         // printf("size: from #%u = %d\n",i,aClFrom[i] >> 8);
      }
      if (aClFlags && aClFlags[i]) {
         iBytes += (aiClFromMaxLen[i]/8)+1;
         // printf("size: from #%u = %d\n",i,aClFrom[i] >> 8);
      }
      if (aClClass[i]) {
         iBytes += 256;
         // printf("size: class#%u = 256\n",i);
      }
   }
   for (int i=0; i<iClToTok; i++)
   {
      if (aClTo && aClToLit && aClToLit[i]) {
         iBytes += (aClTo[i] >> 8);
         // printf("size: to   #%u = %d\n",i,aClTo[i] >> 8);
      }
   }

   int iBrutto = iClFromTok+iClOff;

   iBytes += iBrutto; // aClFrom
   iBytes += iBrutto * sizeof(uint8_t*); // aClData
   if (aClAttr)
   iBytes += iBrutto * sizeof(char*);  // aClAttr
   if (aClInFN)
   iBytes += iBrutto * sizeof(char*);  // aClInFN
   iBytes += iBrutto * sizeof(uint8_t*); // aClFlags
   iBytes += iBrutto * sizeof(uint32_t);   // aClTo
   iBytes += iBrutto * sizeof(uint8_t*);   // aClToLit
   iBytes += iBrutto * sizeof(uint8_t*);   // aClClass
   iBytes += iBrutto * sizeof(int);      // aiClData
   iBytes += iBrutto * sizeof(int);      // aiClBestData
   iBytes += iBrutto * sizeof(int);      // aiClFromMinLen
   iBytes += iBrutto * sizeof(int);      // aiClFromMaxLen
   iBytes += iBrutto * sizeof(uint8_t);    // aClTokOpts
   iBytes += iBrutto * sizeof(uint8_t);    // aClOutOpts
   iBytes += iBrutto;   // aClDynaClass

   // printf("size: ownsize = %d\n", (int)sizeof(*this));

   return
      iBytes
      + (int)sizeof(*this);
      // aClFrom,To,LitBuf,OutBuf,...
}

int SFKMatch::define(char *pszFrom, char *pszTo)
{
   if (provideStatics())
      return 9;

   return defineInt(pszFrom, pszTo);
}

int SFKMatch::defineInt(char *pszFrom, char *pszTo)
{
   if (iClDefAlloc == 0 || iClDefUsed >= iClDefAlloc)
   {
      int iNewAlloc = iClDefAlloc * 2 + 10;

      int iTol = 10;

      char **ppNewFrom = new char*[iNewAlloc+iTol];
      char **ppNewTo   = new char*[iNewAlloc+iTol];
      if (!ppNewFrom || !ppNewTo)
         return 9+sfkerr("out of memory (%d)", iNewAlloc);

      memset(ppNewFrom, 0, sizeof(char*) * (iNewAlloc+iTol));
      memset(ppNewTo  , 0, sizeof(char*) * (iNewAlloc+iTol));

      if (aClDefFrom && aClDefTo && iClDefUsed)
      {
         memcpy(ppNewFrom, aClDefFrom, sizeof(char*) * iClDefUsed);
         memcpy(ppNewTo  , aClDefTo  , sizeof(char*) * iClDefUsed);

         delete [] aClDefFrom;
         delete [] aClDefTo;
      }
 
      aClDefFrom  = ppNewFrom;
      aClDefTo    = ppNewTo;
      iClDefAlloc = iNewAlloc;
   }

   aClDefFrom[iClDefUsed] = strdup(pszFrom);
   aClDefTo[iClDefUsed]   = strdup(pszTo);

   if (!aClDefFrom[iClDefUsed] || !aClDefTo[iClDefUsed])
      return 9+sfkerr("out of memory");

   if (traceLevel() > 0)
      printf("define: %d \"%s\" -> \"%s\"\n",iClDefUsed,aClDefFrom[iClDefUsed],aClDefTo[iClDefUsed]);

   iClDefUsed++;
 
   return 0;
}

int SFKMatch::initClass(uint8_t *pMask, int iMaskSize, const char *pCharsIn)
{
   if (iMaskSize<256)
      { sfkerr("int. #213131"); return 9; }

   memset(pMask, 0, iMaskSize);

   uint8_t *p = (uint8_t *)pCharsIn;

   for (; *p; p++)
      pMask[*p] = 1;

   return 0;
}

void SFKMatch::globalInit( )
{
   provideStatics();
}

int SFKMatch::provideStatics( )
{
   if (!bClStaticInitDone)
   {
      bClStaticInitDone = 1;

      memset(aClSkipMatchChars, 1, sizeof(aClSkipMatchChars));
      aClSkipMatchChars['\0'] = 0;
      aClSkipMatchChars['\r'] = 0;
      aClSkipMatchChars['\n'] = 0;
 
      memset(aClSkipMatchBytes, 1, sizeof(aClSkipMatchBytes));

      initClass(aClSkipMatchWhite , sizeof(aClSkipMatchWhite) , "\t "    );
      initClass(aClSkipMatchXWhite, sizeof(aClSkipMatchXWhite), "\t \r\n");
      initClass(aClSkipMatchAlpha , sizeof(aClSkipMatchAlpha) , "abcdefghijklmnopqrstuvwxyz");
      initClass(aClSkipMatchAlnum , sizeof(aClSkipMatchAlnum) , "abcdefghijklmnopqrstuvwxyz0123456789");
      initClass(aClSkipMatchDigits, sizeof(aClSkipMatchDigits), "0123456789");
      initClass(aClSkipMatchHexDig, sizeof(aClSkipMatchHexDig), "0123456789abcdef");

      memset(&aClFromCopy,  0, sizeof(aClFromCopy));
      memset(&aClPartInfo , 0, sizeof(aClPartInfo));
      memset(&aClPartInfo2, 0, sizeof(aClPartInfo2));
      memset(&aClPrioInfo2, 0, sizeof(aClPrioInfo2));

   // if (defineInt("white",  "chars of (\\t )"      )) return 9;
   // if (defineInt("xwhite", "bytes of (\\t \\r\\n)")) return 9;
   // if (defineInt("alpha",  "bytes of (a-z)"       )) return 9;
   // if (defineInt("alnum",  "bytes of (a-z0-9)"    )) return 9;
   // if (defineInt("digits", "bytes of (0-9)"       )) return 9;
   // if (defineInt("lstart", "start or byte of \\n" )) return 9;
   // if (defineInt("lend"  , "end or byte of \\r\\n")) return 9;
   }

   return 0;
}

int rightClassIncludesLeft(uint8_t *p1,uint8_t *p2,bool &rOverlap,bool &rIncludes)
{
   bool iIncludes=1;
   int  iOverLap=0;

   for (uint32_t i=0; i<256; i++)
   {
      if (p1[i]!=0 && p2[i]==0)
         iIncludes=0;

      if (p1[i]!=0 && p2[i]!=0)
         iOverLap++;
   }

   rOverlap=iOverLap ? 1 : 0;
   rIncludes=iIncludes;

   return (iOverLap || iIncludes) ? 1 : 0;
}

int SFKMatch::isValid( )
{
   return bClIsValid;
}

// RC 0 : OK
// RC 5 : invalid part number referenced (may show part info)
// RC >= 9: any error
int SFKMatch::init(char *pszFromMask, char *pszToMask, int nFlags)
{
   int iSubRC = 0;

   reset();

   if (provideStatics())
      return 9;

   pszClFromText = strdup(pszFromMask);
   bClFromTextWasCopied = 1;

   int iOldLen = strlen(pszFromMask);

   if (!(pszFromMask = expand(pszFromMask)))
      return 9;

   bClAlloc = 0;
   iClAppendLEnd = 0;
   iClTo    = 0;
   iClMaxOrInf = 0;

   bClAttr = (nFlags & SFKMATCH_WITHATTRIB) ? 1 : 0;

   if ((iSubRC = parseFromMask(pszFromMask)))
      return iSubRC;

   if ((iSubRC = parseToMask(pszToMask)))
      return iSubRC;

   if (iClAppendLEnd)
   {
      iClTo++;
   }

   iClToTok = iClTo;
   bClAlloc = 1;
   iClAppendLEnd = 0;
   iClTo    = 0;

   // alloc "to" arrays as they are used by parseFrom
   {
      int iBrutto = iClToTok+iClOff;

      if (!(aClTo = new uint32_t[iBrutto]))
         return 9+sfkerr("outofmem");
      memset(aClTo, 0, sizeof(uint32_t) * iBrutto);

      if (!(aClToLit = new uint8_t*[iBrutto]))
         return 9+sfkerr("outofmem");
      memset(aClToLit, 0, sizeof(uint8_t*) * iBrutto);
   }

   if ((iSubRC = parseFromMask(pszFromMask)))
      return iSubRC;

   if ((iSubRC = parseToMask(pszToMask)))
      return iSubRC;

   if (iClAppendLEnd)
   {
      aClTo[iClTo++] = TokPart | ((iClAppendLEnd) << 8);
   }

   // calc in/out amounts
   {
      iClFromRange = 0;
      for (int i=0; i<iClFromTok; i++)
      {
         if (aClData[i])
            iClFromRange += aiClFromMaxLen[i];
      }
   }

   {
      iClToRange = 0;
      int itok=0,imaxlen=0,ipart=0;
      for (int imask=0; imask<iClToTok; imask++)
      {
         itok    = aClTo[imask] & 0xFFU;
         imaxlen = aClTo[imask] >> 8;
         switch (itok)
         {
            case TokLiteral:
               iClToRange += imaxlen;
               break;
 
            case TokPart:
               ipart = imaxlen-1;
               if (ipart<0 || ipart>=iClFromTok)
                  return 5+sfkerr("invalid part number: %d", imaxlen);
               iClToRange += aiClFromMaxLen[ipart];
               break;

            case TokOutFN:
               iClToRange += iClOutFNMaxLen+10;
               break;
         }
      }
   }

   // auto adapt output buffer size
   int iMaxOut = objectToRange();
   if (iMaxOut > iClOutSizeAlloc) {
      if (aClOutBuf)
         delete [] aClOutBuf;
      aClOutBuf = 0;
      if (aClOutAttr)
         delete [] aClOutAttr;
      aClOutAttr = 0;
      iClOutSizeAlloc = 0;
   }
   if (iMaxOut > iClOutSizeMax)
      iClOutSizeMax = iMaxOut;

   // plausi check
   int iRC=0;

   for (int i=0; i<iClFrom; i++)
   {
      if (!(aClTokOpts[i] & TOKOPT_PRIOMASK))
      {
         sfkerr("internal: missing token prio for part %d", i+1);
         iRC=9;
      }
   }

   bool bOverlap=0;
   bool bIncludes=0;
   for (int i1=0; i1+1<iClFrom; i1++)
   {
      int i2=i1+1;
      int iTok1 = aClFrom[i1];
      int iTok2 = aClFrom[i2];
      if (aiClFromMinLen[i1] != aiClFromMaxLen[i1])
      {
         uint8_t *pClass1=aClClass[i1];
         uint8_t *pClass2=aClClass[i2];
         if (!pClass1 || !pClass2)
            continue;
         uint8_t  iprio1 =aClTokOpts[i1]&TOKOPT_PRIOMASK;
         uint8_t  iprio2 =aClTokOpts[i2]&TOKOPT_PRIOMASK;
         if (   iTok1==TokClass && iTok2==TokClass
             && iprio1 <= iprio2
            )
         {
            if (rightClassIncludesLeft(pClass1,pClass2,bOverlap,bIncludes))
            {
               if (bIncludes)
               {
                  sfkerr("part %d is always empty as part %d will block it: %s", i1+1,i2+1,pszFromMask);
                  sfkinf("part %d includes all characters of part %d and has same or higher priority.\n", i2+1, i1+1);
                  sfkinf("because part %d is of variable length this means it will not collect anything.\n", i1+1);
                  sfkinf("you may rewrite part(s) as [bytes of ...] or [bytes not ...] without overlaps.\n", i2+1);
                  sfkinf("you may insert literals or other commands to search more precisely.\n");
                  iRC=10;
               }
               // else if (bOverlap) {
               //   sfkerr("part %d may not work as part %d uses some of it's chars: %s", i1+1,i2+1,pszFromMask);
               //   bAnyErr=1;
               // }
            }
         }
      }
   }

   if (iRC == 0)
      bClIsValid = 1;

   return iRC;
}

char *SFKMatch::fromText( ) { return pszClFromText; }

int SFKMatch::provideBuffer( )
{
   if (aClOutBuf) {
      if (iClOutSizeAlloc > iClOutSizeMax)
         return 0;
      delete [] aClOutBuf;
      if (aClOutAttr)
         delete [] aClOutAttr;
      aClOutBuf = 0;
      aClOutAttr = 0;
      iClOutSizeAlloc = 0;
   }

   int iTolerance = 100;
   int iBrutto = iClOutSizeMax+iTolerance;

   if (!(aClOutBuf = new uint8_t[iBrutto]))
      return 9+sfkerr("out of memory, cannot create output buffer (%d)", iBrutto);
   memset(aClOutBuf, 0, iBrutto);

   if (!(aClOutAttr = new char[iBrutto]))
      return 9+sfkerr("out of memory, cannot create output buffer (%d)", iBrutto);
   memset(aClOutAttr, 0, iBrutto);

   iClOutSizeAlloc = iClOutSizeMax;

   return 0;
}

int SFKMatch::shutdown( )
{
   if (aClOutBuf) {
      delete [] aClOutBuf;
      aClOutBuf = 0;
      iClOutSizeAlloc = 0;
   }
   if (aClOutAttr) {
      delete [] aClOutAttr;
      aClOutAttr = 0;
   }
   if (aClExpBuf) {
      delete [] aClExpBuf;
      aClExpBuf = 0;
      iClExpBuf = 0;
   }
   for (int i=0; i<iClDefUsed; i++) {
      if (aClDefFrom[i]) delete [] aClDefFrom[i];
      if (aClDefTo[i]) delete [] aClDefTo[i];
   }
   if (aClDefFrom) delete [] aClDefFrom;
   if (aClDefTo) delete [] aClDefTo;
   aClDefFrom = 0;
   aClDefTo = 0;
   return 0;
}

int SFKMatch::verify(int iPattern)
{
   int iRC = 0;

   // plausi check: does to reference important from parts?
   for (int iFrom=0; iFrom<iClFrom; iFrom++)
   {
      int iFromTok = aClFrom[iFrom];

      bool bNeedRef = 0;
      bool bHaveRef = 0;

      if (iFromTok != TokStartOr)
         continue;

      for (int iTo=0; iTo<iClTo; iTo++)
      {
         int iToTokRaw = aClTo[iTo];
         int iToTok    = iToTokRaw & 0xFFU;
         int iToParm   = iToTokRaw >> 8;

         if (iToTok == TokPart && iFrom+1 == iToParm)
            bHaveRef = 1;
      }

      if (!bHaveRef)
      {
         sfkinf("pattern %u: [part%u] is not used in output, may loose line endings.\n", iPattern, iFrom+1);
         iRC = 1;
      }
   }

   return iRC;
}

char *SFKMatch::recentPartInfo()
{
   int i=0,ipart=0,iidx=0;
   uint8_t p=0;
   for (i=0; i<sizeof(aClFromCopy)-10 && aClFromCopy[i]!=0; i++)
   {
      if (!aClPartInfo[i])
         continue;
      ipart = aClPartInfo[i];
      iidx  = ipart-1;
      if (iidx<0 || iidx>=iClFrom)
         continue;
      p = aClTokOpts[iidx] & TOKOPT_PRIOMASK;
      char c = '0'+(ipart%10);
      aClPartInfo2[i] = c;
      aClPrioInfo2[i] = '0'+p;
      for (int k=i; k>=0; k--) {
         if (aClPartInfo2[k]==0) {
            aClPartInfo2[k]=c;
            aClPrioInfo2[k]='0'+p;
         }
      }
   }
   aClPartInfo2[i] = '\0';
   aClPrioInfo2[i] = '\0';
   return (char*)aClPartInfo2;
}

char *SFKMatch::recentPrioInfo()
{
   return (char*)aClPrioInfo2;
}

int SFKMatch::define(uint8_t *pSrcInfo, int iCharOff, int iToken,
   int iminlen, int imaxlen, int iCharClassType,
   uint8_t *pOptData, uint8_t *pOptFlags)
{
   iClRecentDefinedToken  = iToken;
   iClRecentCharClassType = iCharClassType;

   // caller may either supply 0 (char class is of no interest here),
   // 10 (bytes) or 11 (chars) but not 12 (there should be a class
   // but we don't know it)
   if (iCharClassType==12) // i.e. 10+2 (undefined)
      return 9+sfkerr("internal: define() called with undefined char class type: %s", pSrcInfo);

   if (!bClAlloc)
      return 0; // simulation mode

   // trace("define #%u tok=%d len=%d/%d charoff=%d", iClFrom, iToken, iminlen, imaxlen, iCharOff);

   if (aClData[iClFrom])
      return 9+sfkerr("internal: multiple define on same index %d: %s", iClFrom, (char*)pSrcInfo);

   int ilitoff = 0;

   #ifdef SFK_LOR
   // with literals, data contains 1. found result data 2. search pattern
   // with the search pattern after result data at offset ilitoff.
   if (iToken == TokLiteral)
      ilitoff = imaxlen;
   #endif // SFK_LOR

   aClFrom[iClFrom]  = iToken;
   aClData[iClFrom]  = new uint8_t[imaxlen+ilitoff+iClOff];
   if (!aClData[iClFrom])
      return 9+sfkerr("out of memory");
   aiClFromMinLen[iClFrom] = iminlen;
   aiClFromMaxLen[iClFrom] = imaxlen;
   memset(aClData[iClFrom], 0, imaxlen+ilitoff+iClOff);
   if (pOptData)
      memcpy(aClData[iClFrom]+ilitoff, pOptData, imaxlen);

   if (bClAttr) {
      aClAttr[iClFrom] = new char[imaxlen+ilitoff+iClOff];
      if (!aClAttr[iClFrom])
         return 9+sfkerr("out of memory");
      memset(aClAttr[iClFrom], 0, imaxlen+ilitoff+iClOff);
   }

   if (!bClXChars && pOptFlags)
   {
      // flag field covers ONLY the search pattern
      // and therefore never contains ilitoff.
      int iFlagNetto  = (imaxlen/8)+1;
      int iFlagBrutto = iFlagNetto+iClOff;
      if (iFlagNetto > sizeof(aClLitBufBit))
         return 9+sfkerr("int. #213841");
      aClFlags[iClFrom] = new uint8_t[iFlagBrutto];
      if (!aClFlags[iClFrom])
         return 9+sfkerr("out of memory");
      memset(aClFlags[iClFrom], 0, iFlagBrutto);
      memcpy(aClFlags[iClFrom], pOptFlags, iFlagNetto);
   }

   if (iCharOff > 0 && iCharOff < sizeof(aClPartInfo)-10)
      aClPartInfo[iCharOff-1] = iClFrom+1;

   // set default prio if possible
   uint8_t iprio=0;
   switch (iToken)
   {
      case TokStart   : iprio=XPrioAnchor;  break;
      case TokEnd     : iprio=XPrioAnchor;  break; // and "end or byte of"
      case TokStartOr : iprio=XPrioAnchor;  break;
      case TokLStart  : iprio=XPrioAnchor;  break;
      case TokEndOrEOL: iprio=XPrioAnchor;  break;
      case TokLiteral : iprio=XPrioLiteral; break;
      case TokEOL     : iprio=XPrioLiteral; break;
      #ifdef SFKVAR
      case TokSetVar  : iprio=XPrioLiteral; break;
      case TokEndVar  : iprio=XPrioLiteral; break;
      case TokGetVar  : iprio=XPrioLiteral; break;
      #endif
   }
   aClTokOpts[iClFrom] |= iprio;

   if (bClSetKeep)
   {
      bClSetKeep = 0;
      aClTokOpts[iClFrom] |= TOKOPT_KEEP;
   }

   return 0;
}

int SFKMatch::addData(int imask, uint8_t *pSrc, int iSrcLen, char *pSrcAttr)
{
   if (imask < 0)
      return 1;

   if (!aClData[imask])
      return 9+sfkerr("undefined part buffer %d to add to\n", imask);

   int ipartpos = aiClData[imask];
   int imaxlen  = aiClFromMaxLen[imask];

   if (ipartpos+iSrcLen > imaxlen)
      return 9+sfkerr("overflow: part #%u at pos=%d maxlen=%d\n", imask, ipartpos+iSrcLen, imaxlen);

   if (iSrcLen > 0)
   {
      memcpy(aClData[imask]+ipartpos, pSrc, iSrcLen);

      if (bClAttr && aClAttr[imask]) {
         // highlight literals during match
         if (cClLitAttr != 0 && aClFrom[imask] == TokLiteral) {
            memset(aClAttr[imask]+ipartpos, 'i', iSrcLen);
         }
         else
         if (pSrcAttr) {
            memcpy(aClAttr[imask]+ipartpos, pSrcAttr, iSrcLen);
         }
      }
   }

   ipartpos += iSrcLen;

   aiClData[imask] = ipartpos;

   if (ipartpos > aiClBestData[imask])
      aiClBestData[imask] = ipartpos;

   return 0;
}

int getSlashUChar(uint8_t *pSrc, uint16_t &uc, bool bWithRoundBrackets=0)
{
   if (*pSrc != '\\') { // spat.5 getuchar 5
      uc = *pSrc;
      return 1;
   }
 
   switch (pSrc[1])
   {
      case '\\': uc = '\\'; return 2;
      case 't' : uc = '\t'; return 2;
      case 'q' : uc = '"' ; return 2;
      case 'n' : uc = '\n'; return 2;
      case 'r' : uc = '\r'; return 2;
      case '[' : uc = '[';  return 2;
      case ']' : uc = ']';  return 2;
      case '0' : uc = '\0'; return 2;
      case '*' : uc = '*';  return 2;
      case '?' : uc = '?';  return 2;
      case '-' : uc = '-';  return 2;
      case ' ' : uc = ' ';  return 2;
   // case 's' : uc = '$';  return 2;
   // case 'p' : uc = '%';  return 2;
      case '(' : case ')':
         if (bWithRoundBrackets)
            { uc = pSrc[1]; return 2; }
         break;
      case 'x' :
      {
         int iValue = getTwoDigitHex((char*)pSrc+2);
         if (iValue < 0)
            return -1;
         uc = (uint16_t)iValue;
         return 4;
      }
      case 'd' : // sfk 1723 added: "\d255"
      {
         int getThreeDigitDec(char *psz);
         int iValue = getThreeDigitDec((char*)pSrc+2);
         if (iValue < 0)
            return -1;
         uc = (uint16_t)iValue;
         return 5;
      }
   }
 
   // bad sequence
   return -1;
}

void SFKMatch::trace(const char *pszFormat, ...)
{
   if (!traceLevel())
      return;

   char szBuf[300];

   va_list argList;
   va_start(argList, pszFormat);
   ::vsnprintf(szBuf, sizeof(szBuf)-10, pszFormat, argList);
   szBuf[sizeof(szBuf)-10] = '\0';

   int iLen = strlen(szBuf);
   if (iLen > 0 && szBuf[iLen-1] == '\n')
      szBuf[iLen-1] = '\0';

   printf("sfkmatch: %s\n", szBuf);
}

#define INTOK_KET 0x0100U
#define INTOK_BRA 0x0200U
#define INTOK_STA 0x0300U
#define INTOK_QUE 0x0400U
#define INTOK_ROK 0x0500U // ")"

char *SFKMatch::expand(char *pSrcIn)
{
   int iMaxDst = 0;

   // pass 1: calc output size
   if (!expandSub(pSrcIn, 0, &iMaxDst))
      return 0;

   // realloc out buffer on demand
   if (iMaxDst > iClExpBuf)
   {
      if (aClExpBuf) {
         delete [] aClExpBuf;
         aClExpBuf = 0;
         iClExpBuf = 0;
      }

      iMaxDst += 100;

      int iTol = 100;
 
      if (!(aClExpBuf = new uint8_t[iMaxDst+iTol])) {
         sfkerr("out of memory (%d)", iMaxDst);
         return 0;
      }

      memset(aClExpBuf, 0, iMaxDst+iTol);

      iClExpBuf = iMaxDst;
   }

   // pass 2: render output
   char *pres = expandSub(pSrcIn, 1, 0);
   // printf("FROM: \"%s\"\n", pSrcIn);
   // printf("TO  : \"%s\"\n", pres);
   return pres;
}

char *SFKMatch::expandSub(char *pSrcIn, bool bWrite, int *ppMaxDst)
{
   uint8_t *pSrcCur = (uint8_t*)pSrcIn;
   uint8_t *pSrcMax = (uint8_t*)pSrcIn+strlen(pSrcIn);

   uint8_t *pDstCur = aClExpBuf; // or NULL
   uint8_t *pDstMax = aClExpBuf+iClExpBuf;

   int istate=0,iskip=0;
   int iSrcRemain=0,iDstRemain=0;

   uint16_t uc=0;

   while (pSrcCur < pSrcMax)
   {
      iSrcRemain = pSrcMax - pSrcCur;

      if (bWrite)
         iDstRemain = pDstMax - pDstCur;

      uc = pSrcCur[0];

      iskip = 1;

      // if (istate != 1)
      {
         switch (uc)
         {
            case '[': uc = INTOK_BRA; break;
            case ']': uc = INTOK_KET; break;
            case '\\':
               iskip = getSlashUChar(pSrcCur, uc, 1);
               if (iskip < 0)
                  { sfkerr("syntax error while preparing: %s", pSrcCur); return 0; }
         }
      }

      if (istate == 0) // any
      {
         if (uc == INTOK_BRA) {
            istate = 10;
         }
         else if (uc == INTOK_KET) {
            sfkerr("found ] without [: %s", pSrcCur);
            return 0;
         }
         // fall through
      }
      else
      if (istate == 10) // command any
      {
         if (uc == INTOK_KET)
            istate = 0;
         else
         for (int iCmd=iClDefUsed-1; iCmd>=0; iCmd--)
         {
            const char *pFCmd = aClDefFrom[iCmd];
            const char *pTCmd = aClDefTo[iCmd];
            int   iFLen = strlen(pFCmd);
            int   iTLen = strlen(pTCmd);

            if (iSrcRemain < iFLen)
               continue;

            if (!strncmp((char*)pSrcCur, pFCmd, iFLen))
            {
               // printf("REP : %d %s -> %s\n",iCmd,pFCmd,pTCmd);
               if (bWrite)
               {
                  if (iDstRemain < iTLen)
                     { sfkerr("buffer overflow: %s", pSrcIn); return 0; }
                  memcpy(pDstCur, pTCmd, iTLen);
               }
               pDstCur += iTLen;
               pSrcCur += iFLen;
               iskip = 0;
               break;
            }
         }
         if (!iskip)
            continue;
         // fall through
      }

      if (bWrite)
      {
         // copy through unchanged input
         if (iDstRemain < iskip)
            { sfkerr("buffer overflow: %s", pSrcIn); return 0; }
         memcpy(pDstCur, pSrcCur, iskip);
      }
      pDstCur += iskip;
      pSrcCur += iskip;
   }

   if (ppMaxDst)
      *ppMaxDst = (int)(pDstCur-aClExpBuf)+1;

   if (!bWrite)
   {
      // aClExpBuf may not be alloc'ed yet
      return (char*)1;
   }

   // add zero termination on output
   *pDstCur = '\0';
 
   return (char*)aClExpBuf;
}

void SFKMatch::safeStrCopy(char *pszDst, const char *pszSrc, int nMaxDst)
{
   if (nMaxDst < 2) {
      if (nMaxDst >= 1)
         pszDst[0] = '\0';
      return;
   }
   int nLen = strlen(pszSrc);
   if (nLen > nMaxDst-1)
      nLen = nMaxDst-1;
   memcpy(pszDst, pszSrc, nLen);
   pszDst[nLen] = '\0';
}

int SFKMatch::parseFromMask(char *pSrcIn)
{
   if (traceLevel() > 1)
   {
      trace("=== parseFrom.%d: \"%s\" ===", bClAlloc, pSrcIn);
   }

   if (bClAlloc)
   {
      // 2nd pass: allocate resources
      iClFromTok = iClFrom;

      int iBrutto = iClFromTok+iClOff;

      aClFrom = new uint8_t[iBrutto];
      memset(aClFrom, 0, iBrutto);

      aClData = new uint8_t*[iBrutto];
      memset(aClData, 0, sizeof(uint8_t*) * iBrutto);

      if (bClAttr) {
         aClAttr = new char*[iBrutto];
         memset(aClAttr, 0, sizeof(char*) * iBrutto);
      }

      aClInFN = new char*[iBrutto];
      memset(aClInFN, 0, sizeof(char*) * iBrutto);

      aClFlags = new uint8_t*[iBrutto];
      memset(aClFlags, 0, sizeof(uint8_t*) * iBrutto);

      aClClass = new uint8_t*[iBrutto];
      memset(aClClass, 0, sizeof(uint8_t*) * iBrutto);

      aiClData = new int[iBrutto];
      memset(aiClData, 0, sizeof(int) * iBrutto);

      aiClBestData = new int[iBrutto];
      memset(aiClBestData, 0, sizeof(int) * iBrutto);

      aiClFromMinLen = new int[iBrutto];
      memset(aiClFromMinLen, 0, sizeof(int) * iBrutto);

      aiClFromMaxLen = new int[iBrutto];
      memset(aiClFromMaxLen, 0, sizeof(int) * iBrutto);

      aClTokOpts = new uint8_t[iBrutto];
      memset(aClTokOpts, 0, sizeof(uint8_t) * iBrutto);

      aClOutOpts = new uint8_t[iBrutto];
      memset(aClOutOpts, 0, sizeof(uint8_t) * iBrutto);

      aClDynaClass = new uint8_t[iBrutto];
      memset(aClDynaClass, 0, iBrutto);

      #ifdef SFK_LOR
      aClOrInf = new int*[iBrutto];
      memset(aClOrInf, 0, sizeof(int*) * iBrutto);
      #endif // SFK_LOR

      // and render part infos
      safeStrCopy((char*)aClFromCopy, pSrcIn, sizeof(aClFromCopy)-10);
      memset(aClPartInfo, 0, sizeof(aClPartInfo));
      memset(aClPartInfo2, 0, sizeof(aClPartInfo2));
   }

   iClFrom = 0;

   iClRecentDefinedToken  = 0;
   iClRecentCharClassType = 0; // undefined

   // the\tfoo[100*]bar
   // int aElem\[100\]
   uint8_t *pSrc    = (uint8_t*)pSrcIn;
   uint8_t *pSrcCur = (uint8_t*)pSrc;
   uint8_t *pSrcMax = (uint8_t*)(pSrc + strlen((char*)pSrc));
   uint8_t *pSrc10  = pSrcCur;

   int istate=0,ioldstate=0; // any
   int iskip=0,iminlen=0,imaxlen=0,bzerolen=0;
   uint8_t bClassModeChars=0,bClassModeOf=0;
   uint8_t bEscapedAny=0,bEscapedX=0;
 
   uint16_t uc=0,ucLo=0,ucHi=0;
   uint8_t  bInRoundBrackets=0;
   uint8_t *pMaskTemplate=0;

   bClSetKeep = 0;

   while (1)
   {
      trace("match.parse: $%02u %.30s\n", istate, pSrcCur);

      if (pSrcCur >= pSrcMax)
      {
         // end of from mask:
         if (istate == 1)
         {
            // end of /literal/ token, store token
            int iLitLen = iClLitBuf;
            #ifdef SFK_LOR
            if (aClOrInf && aClOrInf[iClFrom]) {
               // complete foo[ortext]bar collection after bar
               int *pOrInf = aClOrInf[iClFrom];
               int iOrPart = pOrInf[0];
               pOrInf[iOrPart] = iClLitBuf;
            }
            #endif // SFK_LOR
            if (define(pSrcCur, pSrcCur-pSrc, TokLiteral, iLitLen, iLitLen, 0, aClLitBuf, aClLitBufBit))
               return 9;
            if (bClAlloc && aClData[iClFrom])
               trace("match.parse: added literal: %s (%d)\n", aClData[iClFrom], iLitLen);
            iClFrom++;
            break;
         }

         if (istate >= 10)
         {
            // cases like /foo[.../ without ] are invalid
            return 9+sfkerr("missing or wrong placed \"]\": %s", pSrcIn);
         }

         // states in range 0 to 9 are outside [] and may stop
         break;
      }

      uc = pSrcCur[0];

      iskip = 1;
      bEscapedAny = 0;
      bEscapedX = 0;

      if (istate==20 || istate==21) // collect char class
      {
         // state 20: directly at start of char listing
         // state 21: within char listing
         switch (uc)
         {
            // * and ? are treated as literals.
            case '[':
               sfkerr("invalid \"[\" within enumeration: %s", pSrcCur);
               sfkinf("use \"\\[\" instead.\n");
               return 9;
            case ']':
               // invalid in closed enum
               if (istate==21 && bInRoundBrackets==1) {
                  sfkerr("invalid \"]\" within enumeration: %s", pSrcCur);
                  sfkinf("use \"\\]\" instead.\n");
                  return 9;
               }
               // valid to end open enum
               uc = INTOK_KET;
               break;
            case '(':
               // invalid in closed enum
               if (istate==21 && bInRoundBrackets==1) {
                  sfkerr("invalid \"(\" within enumeration: %s", pSrcCur);
                  sfkinf("use \"\\(\" instead.\n");
                  return 9;
               }
               break;
            case ')':
               if (bInRoundBrackets)
                  uc = INTOK_ROK;
               break;
            case '\\':
               bEscapedAny = 1;
               if (pSrcCur[1] == 'x' || pSrcCur[1] == 'd')
                  bEscapedX = 1;
               iskip = getSlashUChar(pSrcCur, uc, 1);
               if (iskip < 0)
                  return 9+sfkerr("syntax error within enumeration: %s", pSrcCur);
         }
      }
      else
      if (istate != 10 && istate != 22)
      {
         switch (uc)
         {
            case '[': uc = INTOK_BRA; break;
            case ']': uc = INTOK_KET; break;
            case '*': uc = INTOK_STA; break;
            case '?': uc = INTOK_QUE; break;
            case '\\':
               bEscapedAny = 1;
               if (pSrcCur[1] == 'x' || pSrcCur[1] == 'd')
                  bEscapedX = 1;
               iskip = getSlashUChar(pSrcCur, uc);
               if (iskip < 0)
                  return 9+sfkerr("syntax error in pattern: %s", pSrcCur);
         }
      }

      if (istate == 1 && (uc >= INTOK_BRA && uc <= INTOK_QUE))
      {
         // [ while collecting literal
         #ifdef SFK_LOR
         if (!strncmp((char*)pSrcCur, "[ortext]", 8))
         {
            pSrcCur += 8;
            // sfk1943: iClMaxOrInf will count all ortext globally,
            //    which is actually too large for alloc, but better then too small.
            if (!bClAlloc)
               iClMaxOrInf++;
            if (bClAlloc && !aClOrInf[iClFrom]) {
               aClOrInf[iClFrom] = new int[iClMaxOrInf+4]; // fix sfk1943 NOT iClTok
               memset(aClOrInf[iClFrom], 0, sizeof(int) * (iClMaxOrInf+4));
               aClOrInf[iClFrom][0] = 1;
            }
            if (aClOrInf && aClOrInf[iClFrom]) {
               // set offset/length of current literal OR section
               int *pOrInf = aClOrInf[iClFrom];
               int iOrPart = pOrInf[0];
               pOrInf[iOrPart] = iClLitBuf;
               // printf("# orinf[%d] = %d\n",iOrPart,pOrInf[iOrPart]);
               pOrInf[0]   = pOrInf[0]+1;
            }
            // continue collecting as one long literal
            continue;
         }
         else
         #endif // SFK_LOR
         {
            // literal token end, store token
            int iLitLen = iClLitBuf;
            #ifdef SFK_LOR
            if (aClOrInf && aClOrInf[iClFrom]) {
               // complete foo[ortext]bar collection after bar
               int *pOrInf = aClOrInf[iClFrom];
               int iOrPart = pOrInf[0];
               pOrInf[iOrPart] = iClLitBuf;
            }
            #endif // SFK_LOR
            if (define(pSrcCur, pSrcCur-pSrc, TokLiteral, iLitLen, iLitLen, 0, aClLitBuf, aClLitBufBit))
               return 9;
            if (bClAlloc && aClData[iClFrom])
               trace("match.parse: added literal: %s (%d)\n", aClData[iClFrom], iLitLen);
            iClFrom++;
            istate = 0;
         }
      }

      ioldstate = istate;

      if (istate == 0) // any
      {
         if (uc == INTOK_BRA) {
            // special case: [keep] marker must be exactly:
            if (!strncmp((char*)pSrcCur, "[keep]", 6)) {
               pSrcCur += 6;
               bClSetKeep = 1; // for next define() of a token
               continue;
            }
            istate = 10;
            pSrcCur += iskip;
            pSrc10 = pSrcCur;
            continue;
         }
         else if (uc == INTOK_KET) {
            return 9+sfkerr("found ] without [: %s", pSrcCur);
         }
         else if (uc == INTOK_STA) {
            bClassModeChars = 1;
            pSrcCur += iskip;
            // is it "**" ?
            if (*pSrcCur=='*') {
               pSrcCur++;
               bClassModeChars = 0;
            }
            #ifdef AUTO_REDUCE_WILDCARD
            if (iClFrom > 0 && iClRecentDefinedToken==TokClass) {
               // change [white]*(*) to [white][otherany]
               istate = 16;
            } else
            #endif
            {
               // default is to change * to [chars]
               istate = 3;
            }
            // fall through
         }
         else if (uc == INTOK_QUE) {
            bClassModeChars = 1;
            istate = 4;
            pSrcCur += iskip;
            // fall through
         } else {
            // literal start
            istate = 1;
            iClLitBuf = 0;
            memset(aClLitBufBit, 0, sizeof(aClLitBufBit));
         }
      }

      if (istate == 1) // literal collect
      {
         if (iClLitBuf > SFKMATCH_MAX_LITERAL_SIZE)
            return 9+sfkerr("literal too long: %s", pSrcCur);
         aClLitBuf[iClLitBuf] = uc;
         if (bEscapedX)
            sfkSetBit(aClLitBufBit,iClLitBuf);
         iClLitBuf++;
         pSrcCur += iskip;
         continue;
      }

      if (istate == 10) // command any
      {
         // per command init
         iminlen         = 0;
         imaxlen         = 0;
         bzerolen        = 0;
         bClassModeChars = 2; // undefined
         bClassModeOf    = 0;
         pMaskTemplate   = 0;

         if (isdigit(uc) || uc=='.') {
            iminlen = atoi((char*)pSrcCur);
            while (isdigit(*pSrcCur))
               pSrcCur++;
            if (*pSrcCur=='.') {
               pSrcCur++;
               // 0.300 or 0.*
               if (*pSrcCur=='*') {
                  imaxlen = SFKMatchDefaultMaxLen;
                  pSrcCur++;
               } else {
                  imaxlen = atoi((char*)pSrcCur);
                  while (isdigit(*pSrcCur))
                     pSrcCur++;
               }
            } else {
               imaxlen = iminlen;
            }
            if (!iminlen && !imaxlen)
               bzerolen = 1; // sfk181: support zero length token
            if (*pSrcCur != ' ')
               return 9+sfkerr("missing blank: %s", pSrcCur);
            pSrcCur++;
            istate = 11;
            continue;
         } else {
            imaxlen = SFKMatchDefaultMaxLen;
         }
         if (uc == ',') {
            // next command
            pSrcCur++;
            continue;
         }
         if (uc == ']') {
            // end of command(s)
            pSrcCur++;
            istate = 0;
            continue;
         }
         // else fall through to type
         istate = 11;
      }

      if (istate == 11) // command.type
      {
         /*
            chars|*|bytes
            chars|*|bytes of 0-9
            chars|*|bytes not \t\x20
         */
         if (!strncmp((char*)pSrcCur, "chars", 5)) {
            pSrcCur += 5;
            bClassModeChars = 1;
            istate = 18;
            continue;
         }
         if (pSrcCur[0] == '*') {
            bClassModeChars = 1;
            pSrcCur++;
            // is it "**" ?
            if (*pSrcCur=='*') {
               pSrcCur++;
               bClassModeChars = 0;
            }
            #ifdef AUTO_REDUCE_WILDCARD
            if (iClFrom > 0 && iClRecentDefinedToken==TokClass) {
               // change [white]*(*) to [white][otherany]
               istate = 17;
            } else
            #endif
            {
               // default is to change * to [chars]
               istate = 18;
            }
            continue;
         }
         if (!strncmp((char*)pSrcCur, "bytes", 5)) {
            pSrcCur += 5;
            bClassModeChars = 0;
            istate = 18;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "byte", 4)) {
            pSrcCur += 4;
            bClassModeChars = 0;
            imaxlen = 0; // i.e. fixed length 1
            istate = 18;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "char", 4)) {
            pSrcCur += 4;
            bClassModeChars = 1;
            imaxlen = 0; // i.e. fixed length 1
            istate = 18;
            continue;
         }
         // --- macro classes begin ---
         if (!strncmp((char*)pSrcCur, "white", 5)) {
            pSrcCur += 5;
            pMaskTemplate = aClSkipMatchWhite;
            bClassModeChars = 1; // for possible "others"
            istate = 18;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "xwhite", 6)) {
            pSrcCur += 6;
            pMaskTemplate = aClSkipMatchXWhite;
            bClassModeChars = 0; // for possible "others"
            istate = 18;
            continue;
         }
         /*
         if (!strncmp((char*)pSrcCur, "others", 6)) {
            pSrcCur += 6;
            if (iClRecentCharClassType == 10)
               bClassModeChars = 0;
            else
            if (iClRecentCharClassType == 11)
               bClassModeChars = 1;
            else {
               sfkerr("[others] is not allowed in this context: %s", pSrcIn);
               sfkinf("[others] can be used only after [chars] or [bytes].\n");
               return 9;
            }
            istate = 17;
            continue;
         }
         */
         /*
         if (!strncmp((char*)pSrcCur, "alpha", 5)) {
            pSrcCur += 5;
            pMaskTemplate = aClSkipMatchAlpha;
            istate = 18;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "alnum", 5)) {
            pSrcCur += 5;
            pMaskTemplate = aClSkipMatchAlnum;
            istate = 18;
            continue;
         }
         */
         if (!strncmp((char*)pSrcCur, "digits", 6)) {
            pSrcCur += 6;
            bClassModeChars = 0;
            pMaskTemplate = aClSkipMatchDigits;
            istate = 18;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "digit", 5)) {
            pSrcCur += 5;
            bClassModeChars = 0;
            pMaskTemplate = aClSkipMatchDigits;
            imaxlen = 0; // i.e. fixed length 1
            istate = 18;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "hexdigits", 9)) {
            pSrcCur += 9;
            bClassModeChars = 0;
            pMaskTemplate = aClSkipMatchHexDig;
            istate = 18;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "hexdigit", 8)) {
            pSrcCur += 8;
            bClassModeChars = 0;
            pMaskTemplate = aClSkipMatchHexDig;
            imaxlen = 0; // i.e. fixed length 1
            istate = 18;
            continue;
         }
         // --- macro classes end ---

         /*
         if (!strncmp((char*)pSrcCur, "start or byte of ", 17)) {
            if (iClFrom > 0)
               return 9+sfkerr("[start] is allowed only as first token.");
            pSrcCur += 17;
            if (define(pSrcCur, pSrcCur-pSrc, TokStartOr, 0, 1))
               return 9;
            bClassModeChars = 0;
            bClassModeOf    = 1;
            istate = 20;
            continue;
         }
         */
         if (!strncmp((char*)pSrcCur, "lstart", 6)) {
            pSrcCur += 6;
            if (define(pSrcCur, pSrcCur-pSrc, TokLStart, 0, 2))
               return 9;
            /* oldlstart
            // create CRLF char class
            if (bClAlloc) {
               if (traceLevel() > 2) trace("match.lstart: set %i dynamic",iClFrom);
               if (!(aClClass[iClFrom] = new uint8_t[256]))
                  return 9+sfkerr("out of memory");
               aClDynaClass[iClFrom] = 1;
               uint8_t *pclass = aClClass[iClFrom];
               memset(pclass, 0, 256);
               pclass['\r'] = 1;
               pclass['\n'] = 1;
            }
            */
            // continue to "]"
            iClFrom++;
            istate = 10;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "start", 5)) {
            if (iClFrom > 0)
               return 9+sfkerr("[start] is allowed only as first token.");
            pSrcCur += 5;
            if (define(pSrcCur, pSrcCur-pSrc, TokStart, 0, 1))
               return 9;
            iClFrom++;
            istate = 10;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "skip", 4)) {
            if (iClFrom > 0)
               return 9+sfkerr("[skip] is allowed only as first word.");
            pSrcCur += 4;
            bClIsSkipPattern = 1;
            istate = 10;
            continue;
         }
         /*
         if (!strncmp((char*)pSrcCur, "end or byte of ", 15)) {
            pSrcCur += 15;
            if (define(pSrcCur, pSrcCur-pSrc, TokEnd, 0, 1))
               return 9;
            bClassModeChars = 0;
            bClassModeOf    = 1;
            istate = 20; // steps iClFrom
            continue;
         }
         */
         if (!strncmp((char*)pSrcCur, "end", 3)) {
            pSrcCur += 3;
            if (define(pSrcCur, pSrcCur-pSrc, TokEnd, 0, 1))
               return 9;
            // create dummy char class
            if (bClAlloc) {
               if (traceLevel() > 2) trace("match.class: set %i dynamic",iClFrom);
               if (!(aClClass[iClFrom] = new uint8_t[256]))
                  return 9+sfkerr("out of memory");
               aClDynaClass[iClFrom] = 1;
               uint8_t *pclass = aClClass[iClFrom];
               memset(pclass, 0, 256);
            }
            // continue to "]"
            iClFrom++;
            istate = 10;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "eol", 3)) {
            pSrcCur += 3;
            if (define(pSrcCur, pSrcCur-pSrc, TokEOL, 1, 2))
               return 9;
            iClFrom++;
            istate = 10;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "lend", 4)) {
            pSrcCur += 4;
            if (define(pSrcCur, pSrcCur-pSrc, TokEndOrEOL, 0, 2))
               return 9;
            iClFrom++;
            istate = 10;
            continue;
         }

         return 9+sfkerr("unknown command: \"%s\"", pSrcCur);
      }

      if (istate == 16 || istate == 17) // after "others"
      {
         // get preceeding token
         if (iClFrom < 1)
            return 9+sfkerr("[others] requires a previous character class.\n");

         // printf("others.mode=%d state=%d rec=%d\n",bClassModeChars,istate,iClRecentCharClassType);

         if (imaxlen == 0 && bzerolen == 0) {
            if (define(pSrcCur, pSrcCur-pSrc, TokByte, 1, 1, 10+bClassModeChars))
               return 9;
         } else {
            if (define(pSrcCur, pSrcCur-pSrc, TokClass, iminlen, imaxlen, 10+bClassModeChars))
               return 9;
         }

         if (bClAlloc)
         {
            int iPrev = iClFrom-1;
            uint8_t *pPrevClass = aClClass[iPrev];
            if (!pPrevClass)
               return 9+sfkerr("[others] requires a previous character class: %s", pSrcIn);
            if (traceLevel() > 2) trace("match.other: set %i dynamic",iClFrom);
            if (!(aClClass[iClFrom] = new uint8_t[256]))
               return 9+sfkerr("out of memory");
            aClDynaClass[iClFrom] = 1;
            uint8_t *pclass = aClClass[iClFrom];
            for (int i=0; i<256; i++)
               pclass[i] = pPrevClass[i] ? 0 : 1;
            // otherchars: always exclude \0\r\n
            if (bClassModeChars) {
               pclass[0]    = 0;
               pclass['\r'] = 0;
               pclass['\n'] = 0;
            }
            // plausi check
            int iUsed=0;
            for (int i=0; i<256; i++)
               if (pclass[i])
                  iUsed++;
            if (!iUsed) {
               sfkerr("[others] searches nothing: %s", pSrcIn);
               sfkinf("inversion of the part before others produces an empty character class\n");
               return 9;
            }
            setPrio(iClFrom, aClTokOpts[iPrev] & TOKOPT_PRIOMASK);
         }

         if (istate == 16) {
            // completed "*" or "**" in literal context
            iClFrom++;
            istate = 0;
            continue;
         }

         // continue in command context
         if (!strncmp((char*)pSrcCur," with ", 6)) {
            pSrcCur += 6;
            bClassModeOf = 1;
            istate = 19;   // [* with ...]
            continue;
         }
         if (!strncmp((char*)pSrcCur," not ", 5)) {
            pSrcCur += 5;
            bClassModeOf = 0;
            istate = 19;   // [* not ...]
            continue;
         }

         iClFrom++;
         istate = 10;
         continue;
      }

      if (istate == 18) // after "chars","bytes","byte","white"
      {
         if (imaxlen == 0 && bzerolen == 0) {
            if (define(pSrcCur, pSrcCur-pSrc, TokByte, 1, 1, 10+bClassModeChars))
               return 9;
         } else {
            if (define(pSrcCur, pSrcCur-pSrc, TokClass, iminlen, imaxlen, 10+bClassModeChars))
               return 9;
         }

         if (!pMaskTemplate)
         {
            if (!strncmp((char*)pSrcCur," of ", 4)) {
               pSrcCur += 4;
               bClassModeOf = 1;
               setPrio(iClFrom, XPrioWhiteList);
               istate = 20;
               continue;
            }
            if (!strncmp((char*)pSrcCur," not ", 5)) {
               pSrcCur += 5;
               bClassModeOf = 0;
               setPrio(iClFrom, XPrioBlackList);
               istate = 20;
               continue;
            }
         }

         // "chars","bytes" without user defined codes:
         // set a pointer to default char class
         if (bClAlloc) {
            if (traceLevel() > 2) trace("match.class: set %i static",iClFrom);
            if (pMaskTemplate) {
               if (!(aClClass[iClFrom] = pMaskTemplate))  // white
                  return 19+sfkerr("int. #2144292");
               setPrio(iClFrom, XPrioWhiteList);
            }
            else
            if (bClassModeChars) {
               aClClass[iClFrom] = aClSkipMatchChars;
               aClTokOpts[iClFrom] |= TOKOPT_LEAN; // chars plain
               setPrio(iClFrom, XPrioWildCard);
            } else {
               aClClass[iClFrom] = aClSkipMatchBytes;
               aClTokOpts[iClFrom] |= TOKOPT_LEAN; // bytes plain
               setPrio(iClFrom, XPrioWildCard);
            }
            aClDynaClass[iClFrom] = 0;
         }

         iClFrom++;
         istate = 10;
         continue;
      }

      if (istate == 19) // others ... of/not
      {
         if (bEscapedAny==0 && uc=='(') {
            bInRoundBrackets = 1;
            if (pSrcCur[iskip]==')') {
               sfkerr("empty character list not allowed: %s", pSrcCur);
               sfkinf("use \\(\\) or )( to select brackets as literals\n");
               return 9;
            }
            pSrcCur += iskip;
         } else {
            bInRoundBrackets = 0;
         }
         istate = 21;
         continue;
      }

      if (istate == 20) // new char class (open)
      {
         if (bClAlloc) {
            if (traceLevel() > 2) trace("match.class: set %i dynamic",iClFrom);
            if (!(aClClass[iClFrom] = new uint8_t[256]))
               return 9+sfkerr("out of memory");
            aClDynaClass[iClFrom] = 1;
            uint8_t *pclass = aClClass[iClFrom];
            if (bClassModeOf) {
               memset(pclass, 0, 256);
            } else {
               memset(pclass, 1, 256);
               if (bClassModeChars) {
                  pclass['\0'] = 0;
                  pclass['\r'] = 0;
                  pclass['\n'] = 0;
               }
            }
         }
         if (bEscapedAny==0 && uc=='(') {
            bInRoundBrackets = 1;
            if (pSrcCur[iskip]==')') {
               sfkerr("empty character list not allowed: %s", pSrcCur);
               sfkinf("use \\(\\) or )( to select brackets as literals\n");
               return 9;
            }
            pSrcCur += iskip;
         } else {
            bInRoundBrackets = 0;
         }
         istate = 21;
         continue;
      }

      if (istate == 21) // collect char class (still open)
      {
         // \x00-\x08
         if (pSrcCur[iskip] == '-') {
            if (*pSrcCur!='\\' && !isalnum((char)uc)) {
               sfkerr("bad range start: %s", pSrcCur);
               sfkinf("use \\x%02x- to start a range with non-alphanumeric character\n", uc);
               sfkinf("use \\- to use literal '-' in this enumeration\n");
               return 9;
            }
            uint8_t *pOld = pSrcCur;
            ucLo = uc;
            ucHi = 0;
            pSrcCur += iskip + 1;
            if (*pSrcCur!='\\' && !isalnum((char)*pSrcCur)) {
               sfkerr("bad range end: %s", pOld);
               sfkinf("use \\x%02x-\\x%02x for a range with non-alphanumeric characters\n", uc, *pSrcCur);
               sfkinf("use \\- to use literal '-' in this enumeration\n");
               return 9;
            }
            if ((iskip = getSlashUChar((uint8_t*)pSrcCur, ucHi)) < 0)
               return 9+sfkerr("bad sequence: %s", pSrcCur);
            ucLo &= 0xFFU;
            ucHi &= 0xFFU;
            if (ucHi < ucLo)
               return 9+sfkerr("range end lower than start: %s (%X-%X)", pOld, ucLo, ucHi);
            if (bClAlloc) {
               if (!aClClass[iClFrom])
                  return 9+sfkerr("internal: class not allocated (21.1)");
               if (bEscapedAny || bClUseCase)
                  memset(&aClClass[iClFrom][ucLo], bClassModeOf, (ucHi-ucLo)+1);
               else {
                  // expand a-z to a-zA-Z (case insensitive)
                  for (; ucLo <= ucHi; ucLo++) {
                     aClClass[iClFrom][sfktolower(ucLo)&0xFF] = bClassModeOf;
                     aClClass[iClFrom][sfktoupper(ucLo)&0xFF] = bClassModeOf;
                  }
               }
            }
            pSrcCur += iskip;
            continue;
         }
         {
            int iCharOff = pSrcCur-pSrc;
            if (iCharOff > 0 && iCharOff < sizeof(aClPartInfo)-10)
               aClPartInfo[iCharOff-1] = iClFrom+1;
         }
         if (bInRoundBrackets==1 && uc==INTOK_ROK) {
            bInRoundBrackets = 0;
            pSrcCur += iskip;
            iClFrom++;
            istate = 10;
            continue;
         }
         if (uc == INTOK_KET) {
            iClFrom++;
            pSrcCur += iskip;
            istate = 0;
            continue;
         }
         if (bClAlloc) {
            if (!aClClass[iClFrom])
               return 9+sfkerr("internal: class not allocated (21.2)");
            aClClass[iClFrom][uc&0xFF] = bClassModeOf;
         }
         pSrcCur += iskip;
         continue;
      }

      if (istate == 22) // add "or \n"
      {
         if (bClAlloc) {
            if (traceLevel() > 2) trace("match.class: set %i dynamic",iClFrom);
            if (!(aClClass[iClFrom] = new uint8_t[256]))
               return 9+sfkerr("out of memory");
            aClDynaClass[iClFrom] = 1;
            uint8_t *pclass = aClClass[iClFrom];
            memset(pclass, 0, 256);
            pclass['\n'] = 1;
         }
         iClFrom++;
         istate = 10;
         continue;
      }

      if (istate == 3) // expand "*(*)" to "[*(*)]"
      {
         if (SFKMatchByteWildCards)
            bClassModeChars = 0;
         if (define(pSrcCur, pSrcCur-pSrc, TokClass, 0, SFKMatchDefaultMaxLen, 10+bClassModeChars))
            return 9;
         if (bClAlloc) {
            if (traceLevel() > 2) trace("match.class: set %i static",iClFrom);
            aClClass[iClFrom] = bClassModeChars ? aClSkipMatchChars : aClSkipMatchBytes;
            aClDynaClass[iClFrom] = 0;
            setPrio(iClFrom, XPrioWildCard);
         }
         iClFrom++;
         istate = 0;
         continue;
      }

      if (istate == 4) // expand "???" to "[3.3 char]"
      {
         int iWidth=1;
         while (pSrcCur<pSrcMax && *pSrcCur=='?')
            { pSrcCur++; iWidth++; }
         if (SFKMatchByteWildCards)
            bClassModeChars = 0;
         if (define(pSrcCur, pSrcCur-pSrc, TokByte, iWidth, iWidth, 10+bClassModeChars))
            return 9;
         if (bClAlloc) {
            if (traceLevel() > 2) trace("match.class: set %i static",iClFrom);
            aClClass[iClFrom] = bClassModeChars ? aClSkipMatchChars : aClSkipMatchBytes;
            aClDynaClass[iClFrom] = 0;
            setPrio(iClFrom, XPrioWildCard);
         }
         iClFrom++;
         istate = 0;
         continue;
      }
   }

   if (bClAlloc && iClFrom)
      trace("match.init: tokens=%d lasttok=0x%X\n", iClFrom, aClFrom[iClFrom-1]);

   // plausi check
   // if (bClAlloc != 0 && iClFrom > 0 && aClFrom[0] == TokEnd)
   //    return 9+sfkerr("[end] must be preceeded by another command like [1 byte].");
   if (bClAlloc != 0 && iClFrom > 0 && aClFrom[0] == TokEndOrEOL)
   {
      sfkerr("[lend] must be preceeded by another command like [1 byte].");
      sfkinf("use [eol] to process (CR)LF line endings.\n");
      sfkinf("use [lstart] to search from line starts.\n");
      return 9;
   }

   // build the head match table
   if (bClAlloc)
   {
      memset(aClHeadMatch, 1, sizeof(aClHeadMatch));

      uint8_t ucFirst = 0;

      int iFirstTok = aClFrom[0];
      int iFirstMin = aiClFromMinLen[0];
      int iFirstLen = aiClFromMaxLen[0];

      switch (iFirstTok)
      {
         case TokLiteral:
         {
            int ilitoff = 0;
            #ifdef SFK_LOR
            ilitoff = iFirstLen;
            #endif // SFK_LOR
            if (!aClData[0]) return 9+sfkerr("int. #2137241");
            if (iFirstLen<1) return 9+sfkerr("int. #2137242");
            memset(aClHeadMatch, 0, sizeof(aClHeadMatch));
            #ifdef SFK_LOR
            int imask = 0;
            if (aClOrInf[imask]) {
               // build headmatch for [ortext] literal group
               int *pOrInf = aClOrInf[imask];
               int iSubStr = 1; // ONE based!
               int nSubStr = pOrInf[0];
               int iMaxLen = iFirstLen;
               int iCurOff = 0;
               int iCurMax = 0;
               int iCurLen = 0;
               int isubrc  = 5; // no match
               while (iSubStr<=nSubStr && iCurOff<iMaxLen)
               {
                  // get current substring
                  iCurMax = pOrInf[iSubStr];
                  iCurLen = iCurMax - iCurOff;
                  // add current [ortext] start to head match
                  ucFirst = aClData[0][0+ilitoff+iCurOff];
                  if (bClUseCase) {
                     aClHeadMatch[ucFirst] = 1;
                  } else {
                     // aClHeadMatch[sfktoupper(ucFirst)&0xFF] = 1;
                     // aClHeadMatch[sfktolower(ucFirst)&0xFF] = 1;
                     sfkSetHeadMatch(ucFirst, aClHeadMatch); // sfk190
                  }
                  // goto next, if any
                  iCurOff += iCurLen;
                  iSubStr++;
               }
            }
            else
            #endif // SFK_LOR
            {
               ucFirst = aClData[0][0+ilitoff];
               if (bClUseCase) {
                  aClHeadMatch[ucFirst] = 1;
               } else {
                  // aClHeadMatch[sfktoupper(ucFirst)&0xFF] = 1;
                  // aClHeadMatch[sfktolower(ucFirst)&0xFF] = 1;
                  sfkSetHeadMatch(ucFirst, aClHeadMatch); // sfk190
               }
            }
            break;
         }

         case TokByte:
         case TokClass:
            if (!aClClass[0]) return 9+sfkerr("int. #2137243");
            if (iFirstMin > 0) // FIX: 173: zero length never matched
               memcpy(aClHeadMatch, aClClass[0], 256);
            break;

         case TokStart:
            // is evaluated only once at file start.
            memset(aClHeadMatch, 0, sizeof(aClHeadMatch));
            break;

         case TokStartOr:
         case TokLStart:
         case TokEnd:
            if (aClClass[0]) {
               memcpy(aClHeadMatch, aClClass[0], 256);
               trace("tokstartor/end copied headmatch");
            }
            break;

         case TokEOL:
            memset(aClHeadMatch, 0, sizeof(aClHeadMatch));
            aClHeadMatch['\r'] = 1;
            aClHeadMatch['\n'] = 1;
            break;
      }
      if (traceLevel() > 1)
      {
         printf("using headmatch:\n");
         for (int i=0; i<256; i++) {
            printf("%c",aClHeadMatch[i]?'+':'.');
            if ((i & 63)==63)
               printf("\n");
         }
         printf("\n");
      }
   }

   return 0;
}

int SFKMatch::parseToMask(char *pSrc)
{
   // the\tfoo[part1]bar
   // int aElem\[part1\]
   uint8_t *pSrcCur = (uint8_t*)pSrc;
   uint8_t *pSrcMax = (uint8_t*)(pSrc + strlen(pSrc));

   int istate=0,ioldstate=0; // any
   int iskip=0,imaxlen=0;
   int ipart=0,iifopen=0;

   #ifdef SFKVAR
   int bopenvar=0;
   #endif
 
   uint16_t uc=0;
 
   bool bDoneAll=0;

   while (1)
   {
      if (bClAlloc)
         trace("to.parse: $%02u %.30s\n", istate, pSrcCur);

      if (pSrcCur >= pSrcMax)
      {
         if (istate == 1)
         {
            if (bClAlloc) {
               // literal token end, store token
               int iLitLen = iClLitBuf;
               if (!(aClToLit[iClTo] = new uint8_t[iLitLen+1]))
                  return 9+sfkerr("out of memory");
               memcpy(aClToLit[iClTo], aClLitBuf, iLitLen);
               aClToLit[iClTo][iLitLen] = '\0'; // safety
               aClTo[iClTo] = TokLiteral | (iLitLen << 8);
               trace("to.parse: added literal: %s (%d)\n",
                  aClToLit[iClTo], iLitLen);
            }
            iClTo++;
         }

         break;
      }

      uc = pSrcCur[0];

      iskip = 1;

      if (istate != 10)
      {
         if (uc == '[')
            uc = INTOK_BRA;
         else
         if (uc == ']')
            uc = INTOK_KET;
         else
         if (uc == '\\') {
            iskip = getSlashUChar(pSrcCur, uc);
            if (iskip < 0)
               return 9+sfkerr("syntax error: %s", pSrcCur);
         }
      }

      if (istate == 1 && uc == INTOK_BRA)
      {
         // literal token end, store token
         if (bClAlloc) {
            int iLitLen = iClLitBuf;
            if (!(aClToLit[iClTo] = new uint8_t[iLitLen+1]))
               return 9+sfkerr("out of memory");
            memcpy(aClToLit[iClTo], aClLitBuf, iLitLen);
            aClToLit[iClTo][iLitLen] = '\0'; // safety
            aClTo[iClTo] = TokLiteral | (iLitLen << 8);
            trace("to.parse: added literal: %s (%d)\n",
               aClToLit[iClTo], iLitLen);
         }
         iClTo++;
         istate = 0;
      }

      ioldstate = istate;

      if (istate == 0) // any
      {
         if (uc == INTOK_BRA) {
            istate = 10;
            pSrcCur += iskip;
            continue;
         }
         if (uc == INTOK_KET)
            return 9+sfkerr("found ] without [: %s", pSrcCur);
         // literal start
         istate = 1;
         iClLitBuf = 0;
      }

      if (istate == 1) // literal collect
      {
         if (iClLitBuf > SFKMATCH_MAX_LITERAL_SIZE)
            return 9+sfkerr("literal too long: %s", pSrcCur);
         aClLitBuf[iClLitBuf++] = uc;
         pSrcCur += iskip;
         continue;
      }

      if (istate == 10) // command any
      {
         // expect maxlength, type or ]
         if (isdigit(uc)) {
            return 9+sfkerr("unexpected number: %s", pSrcCur);
         }
         if (uc == ',') {
            // next command
            pSrcCur++;
            continue;
         }
         if (uc == ']') {
            // end of command(s)
            pSrcCur++;
            istate = 0;
            continue;
         }
         // else fall through to type
         istate = 11;
      }

      if (istate == 11) // command.type
      {
         if (!strncmp((char*)pSrcCur, "parts ", 6)) {
            pSrcCur += 6;
            while (1) {
               // store part token list
               ipart = atoi((char*)pSrcCur);
               if (bClAlloc)
                  aClTo[iClTo] = TokPart | (ipart << 8);
               iClTo++;
               while (isdigit(*pSrcCur))
                  pSrcCur++;
               // apply a part range?
               if (pSrcCur[0]=='-') {
                  pSrcCur++;
                  int iEndPart = atoi((char*)pSrcCur);
                  if (iEndPart < ipart)
                     return 9+sfkerr("invalid part range end: %s", pSrcCur);
                  // fill in range parts
                  ipart++;
                  for (; ipart <= iEndPart; ipart++) {
                     // early range end check to avoid huge array
                     if (bClAlloc==0 && ipart > iClFrom)
                        return 9+sfkerr("invalid part range: %d", ipart);
                     if (bClAlloc)
                        aClTo[iClTo] = TokPart | (ipart << 8);
                     iClTo++;
                  }
                  while (isdigit(*pSrcCur))
                     pSrcCur++;
                  break;
               }
               // continue part enumeration?
               if (pSrcCur[0]!=',')
                  break;
               if (!isdigit(pSrcCur[1]))
                  break;
               pSrcCur++;
            }
            istate = 10;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "part", 4)) {
            // store part token
            pSrcCur += 4;
            if (*pSrcCur==' ')
               pSrcCur++;
            ipart = atoi((char*)pSrcCur);
            if (bClAlloc)
               aClTo[iClTo] = TokPart | (ipart << 8);
            iClTo++;
            istate = 10;
            while (isdigit(*pSrcCur))
               pSrcCur++;
            continue;
         }
         #ifdef SFKVAR
         if (!strncmp((char*)pSrcCur, "setvar ", 7)) {
            if (bopenvar)
               return 9+sfkerr("missing [endvar] before %s",pSrcCur);
            pSrcCur += 7;
            // get variable name
            int iNameLen = 0;
            while (pSrcCur[iNameLen]!=0 && pSrcCur[iNameLen]!=']')
               iNameLen++;
            if (pSrcCur[iNameLen]!=']')
               return 9+sfkerr("missing ] after setvar");
            // store name as literal
            if (bClAlloc) {
               int iLitLen = iClLitBuf;
               if (!(aClToLit[iClTo] = new uint8_t[iNameLen+1]))
                  return 9+sfkerr("out of memory");
               memcpy(aClToLit[iClTo], pSrcCur, iNameLen);
               aClToLit[iClTo][iNameLen] = '\0';
               aClTo[iClTo] = TokSetVar | (iNameLen << 8);
               trace("to.parse: added setvar: %s (%d)\n",
                  aClToLit[iClTo], iNameLen);
            }
            pSrcCur += iNameLen;
            bopenvar = 1;
            iClTo++;
            istate = 10;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "endvar", 6)) {
            if (!bopenvar)
               return 9+sfkerr("[endvar] requires previous [setvar]");
            bopenvar = 0;
            pSrcCur += 6;
            if (*pSrcCur!=']')
               return 9+sfkerr("missing ] after endvar");
            if (bClAlloc) {
               aClTo[iClTo] = TokEndVar;
               trace("to.parse: added endvar\n");
            }
            iClTo++;
            istate = 10;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "getvar ", 7)) {
            pSrcCur += 7;
            // get variable name
            int iNameLen = 0;
            while (pSrcCur[iNameLen]!=0 && pSrcCur[iNameLen]!=']')
               iNameLen++;
            if (pSrcCur[iNameLen]!=']')
               return 9+sfkerr("missing ] after getvar");
            // store name as literal
            if (bClAlloc) {
               int iLitLen = iClLitBuf;
               if (!(aClToLit[iClTo] = new uint8_t[iNameLen+1]))
                  return 9+sfkerr("out of memory");
               memcpy(aClToLit[iClTo], pSrcCur, iNameLen);
               aClToLit[iClTo][iNameLen] = '\0';
               aClTo[iClTo] = TokGetVar | (iNameLen << 8);
               trace("to.parse: added getvar: %s (%d)\n",
                  aClToLit[iClTo], iNameLen);
            }
            pSrcCur += iNameLen;
            iClTo++;
            istate = 10;
            continue;
         }
         #endif
         if (!strncmp((char*)pSrcCur, "strip(part", 10)) {
            // store part token
            pSrcCur += 10;
            if (*pSrcCur==' ')
               pSrcCur++;
            ipart = atoi((char*)pSrcCur);
            if (bClAlloc)
               aClTo[iClTo] = TokPart | (ipart << 8);
            istate = 10;
            while (isdigit(*pSrcCur))
               pSrcCur++;
            // must end with ",\0)"
            if (strncmp((char*)pSrcCur, ",\\0)", 4))
               return 9+sfkerr("strip(... accepts only ,\0) to strip null bytes");
            pSrcCur += 4;
            if (bClAlloc)
               aClOutOpts[iClTo] |= 1;
            iClTo++;
            continue;
         }
         if (!strncmp((char*)pSrcCur, "all", 3)) {
            // store all part tokens
            for (int ipart=1; ipart<=iClFrom; ipart++)
            {
               if (bClAlloc) {
                  if (ipart==1 && aClFrom[0]==TokLStart)
                     continue;
                  if (ipart==iClFrom && aClFrom[ipart-1]==TokEndOrEOL)
                     continue;
                  aClTo[iClTo] = TokPart | (ipart << 8);
               }
               iClTo++;
            }
            istate = 10;
            pSrcCur += 3;
            bDoneAll=1;
            continue;
         }
         // user defined type?
         if (pClOutFN) {
            int iIOLen = strlen((char*)pSrcCur);
            int iMaxLen = 0;
            if (!pClOutFN(1, (char*)pSrcCur, &iIOLen, 0, &iMaxLen))
            {
               // max outlen must be set
               if (iMaxLen < 1)
                  return 9+sfkerr("expression output function did not set max length for: %s", pSrcCur);
               if (iMaxLen > iClOutFNMaxLen)
                  iClOutFNMaxLen = iMaxLen;
               // store similar to a literal.
               // TODO: maxlen is different to literal.
               if (bClAlloc) {
                  int iLitLen = iIOLen;
                  if (!(aClToLit[iClTo] = new uint8_t[iLitLen+1]))
                     return 9+sfkerr("out of memory");
                  memcpy(aClToLit[iClTo], pSrcCur, iLitLen);
                  aClToLit[iClTo][iLitLen] = '\0'; // safety
                  aClTo[iClTo] = TokOutFN | (iLitLen << 8);
                  trace("to.parse: added outfn: %s (%d)\n",
                     aClToLit[iClTo], iLitLen);
               }
               iClTo++;
               istate = 10;
               pSrcCur += iIOLen;
               continue;
            }
         }
         sfkerr("unknown command in totext: \"%s\"", pSrcCur);
         return 9;
      }
   }

   bClToTextIsCover = strcmp(pSrc, "[all]") ? 0 : 1;

   trace("to.init: %d tokens\n", iClTo);

   return 0;
}

int SFKMatch::classcmp(int ipart, uint8_t *pSrc, uint8_t *aClass, int ilen)
{
   for (int i=0; i<ilen; i++)
      if (!aClass[pSrc[i]])
         return 1;
   return 0;
}

char *SFKMatch::classAsTrace(uint8_t *aSkipMatch, int iMax)
{
   static char szBuf[300];
   for (int i=0; i<iMax; i++)
      szBuf[i] = aSkipMatch[i] ? '+':'.';
   szBuf[iMax] = '\0';
   return szBuf;
}

#ifdef SFKINT
int SFKMatch::calcLength(int imask, uint8_t iopt)
{
   return 0; // not used

   if (imask<0 || imask>=iClFrom)
      return -1;
   if (!aClData || !aiClData)
      return -2;

   uint8_t *pdata = 0;
   int    idata = 0;

   char  *pcmd  = aClInFN[imask];
   if (!pcmd)
      return -4;

   /*
      swap(tail(part1,4))-8
   */

   bool  bSwap =   0;
   int   ipart =  -1;
   int   ibra  =   1; // within ()
   int   iAdd  =   0;
   int   iMul  =   0;
   int   iDiv  =   0;
   int   iTail =   0;
   int   iSubOff = -1;
   int   iSubLen = -1;
   int   iStrConv= 0;
   int   istate=   0;

   while (*pcmd)
   {
      if (!strncmp(pcmd, "tail(", 5)) {
         ibra++;
         pcmd += 5;
         istate = 1;
         continue;
      }
      if (!strncmp(pcmd, "subset(", 7)) {
         ibra++;
         pcmd += 7;
         istate = 2;
         continue;
      }
      if (!strncmp(pcmd, "strtoi(", 7)) {
         ibra++;
         pcmd += 7;
         iStrConv = 1;
         continue;
      }
      if (!strncmp(pcmd, "hextoi(", 7)) {
         ibra++;
         pcmd += 7;
         iStrConv = 2;
         continue;
      }
      if (!strncmp(pcmd, "octtoi(", 7)) {
         ibra++;
         pcmd += 7;
         iStrConv = 3;
         continue;
      }
      if (!strncmp(pcmd, "leftpart", 8)) {
         pcmd += 8;
         ipart = atoi(pcmd);
         ipart = imask - ipart;
         while (*pcmd && isdigit(*pcmd))
            pcmd++;
         continue;
      }
      if (!strncmp(pcmd, "part", 4)) {
         pcmd += 4;
         ipart = atoi(pcmd);
         if (ipart<1)
            return -8+sfkerr("invalid part reference: %s", aClInFN[imask]);
         ipart--;
         while (*pcmd && isdigit(*pcmd))
            pcmd++;
         continue;
      }
      switch (istate)
      {
         case 1: // tail
            if (*pcmd!=',')
               return -8+sfkerr("invalid tail statement: %s", aClInFN[imask]);
            pcmd++;
            iTail = atoi(pcmd);
            while (*pcmd && isdigit(*pcmd))
               pcmd++;
            istate = 0;
            continue;
         case 2: // sub
            if (*pcmd!=',')
               return -8+sfkerr("invalid sub statement: %s", aClInFN[imask]);
            pcmd++;
            iSubOff = atoi(pcmd);
            if (*pcmd=='-')
               pcmd++;
            while (*pcmd && isdigit(*pcmd))
               pcmd++;
            if (*pcmd!=',')
               return -8+sfkerr("invalid sub statement: %s", aClInFN[imask]);
            pcmd++;
            iSubLen = atoi(pcmd);
            while (*pcmd && isdigit(*pcmd))
               pcmd++;
            istate = 0;
            continue;
      }
      if (!strncmp(pcmd, "swap(", 5)) {
         ibra++;
         bSwap = 1;
         pcmd += 5;
         continue;
      }
      switch (*pcmd)
      {
         case ')':
            ibra--;
            pcmd++;
            continue;
         case '*':
            if (ibra>1)  { sfkerr("allowed only at top level: %.20s", pcmd); break; }
            if (iAdd!=0) { sfkerr("not allowed after -/+: %.20s", pcmd); break; }
            iMul = atoi(pcmd+1);
            // ignored if 0
            pcmd++;
            while (*pcmd && isdigit(*pcmd))
               pcmd++;
            continue;
         case '/':
            if (ibra>1)  { sfkerr("allowed only at top level: %.20s", pcmd); break; }
            if (iAdd!=0) { sfkerr("not allowed after -/+: %.20s", pcmd); break; }
            iDiv = atoi(pcmd+1);
            // ignored if 0
            pcmd++;
            while (*pcmd && isdigit(*pcmd))
               pcmd++;
            continue;
         case '+':
            if (ibra>1)  { sfkerr("allowed only at top level: %.20s", pcmd); break; }
            iAdd = atoi(pcmd+1);
            pcmd++;
            while (*pcmd && isdigit(*pcmd))
               pcmd++;
            continue;
         case '-':
            if (ibra>1)  { sfkerr("allowed only at top level: %.20s", pcmd); break; }
            iAdd = 0 - atoi(pcmd+1);
            pcmd++;
            while (*pcmd && isdigit(*pcmd))
               pcmd++;
            continue;
      }
      return -5+sfkerr("unexpected: \"%.5s\" within: \"%s\"", pcmd, aClInFN[imask]);
   }

   if (ipart<0 || ipart>=iClFrom)
      return -6+sfkerr("invalid part%d referenced in: %s", ipart+1, aClInFN[imask]);

   if (bSwap && iStrConv)
      return -11+sfkerr("swap and strtoi cannot be combined: %s", aClInFN[imask]);

   pdata = aClData[ipart];
   idata = aiClData[ipart];

   if (!pdata)
      return -7+sfkerr("part%d has no data", ipart+1);

   if (iSubLen > 0)
   {
      if (iSubOff < 0)
         iSubOff = idata + iSubOff;

      if (iSubOff < 0 || iSubOff+iSubLen > idata)
         return -9+sfkerr("sub offset/length does not fit (%d,%d/%d): %s", iSubOff,iSubLen, idata, aClInFN[imask]);

      pdata += iSubOff;
      idata  = iSubLen;
   }
   else
   if (iTail > 0)
   {
      if (iTail > idata)
         return -9+sfkerr("tail length too large (%d/%d): %s", iTail, idata, aClInFN[imask]);
 
      pdata += idata - iTail;
      idata  = iTail;
   }

   int ilen = 0;

   if (iStrConv)
   {
      switch (iStrConv)
      {
         case 1: ilen = atoi((char*)pdata); break;
         case 2: ilen = strtol((char*)pdata, 0, 0x10); break;
         case 3: ilen = strtol((char*)pdata, 0, 8); break;
      }
   }
   else
   switch (idata)
   {
      case 1:
         ilen = pdata[0];
         break;
      case 2:
         if (bSwap)
            ilen = (int)( (((uint32_t)pdata[1])<<8) | ((uint32_t)pdata[0]) );
         else
            ilen = (int)( (((uint32_t)pdata[0])<<8) | ((uint32_t)pdata[1]) );
         break;
      case 3:
         if (bSwap)
            ilen = (int)( (((uint32_t)pdata[2])<<16) |
                          (((uint32_t)pdata[1])<<8)  | ((uint32_t)pdata[0]) );
         else
            ilen = (int)( (((uint32_t)pdata[0])<<16) |
                          (((uint32_t)pdata[1])<<8)  | ((uint32_t)pdata[2]) );
         break;
      case 4:
         if (bSwap)
            ilen = (int)( (((uint32_t)pdata[3])<<24) | (((uint32_t)pdata[2])<<16) |
                          (((uint32_t)pdata[1])<< 8) | ((uint32_t)pdata[0]) );
         else
            ilen = (int)( (((uint32_t)pdata[0])<<24) | (((uint32_t)pdata[1])<<16) |
                          (((uint32_t)pdata[2])<< 8) | ((uint32_t)pdata[3]) );
         break;
      default:
         sfkerr("part%d has unexpected data length: %d",imask+1,idata);
         return -3;
   }

   if (iMul > 0)
      ilen *= iMul;

   if (iDiv > 0)
      ilen /= iDiv;

   ilen += iAdd;

   // printf("len from part%d = %d (0x%X) tail=%d\n",ipart+1,ilen,ilen,iTail);

   return ilen;
}
#endif // SFKINT

void SFKMatch::setPrio(int imask, uint8_t iprio)
{
   if (!bClAlloc)
      return;
   aClTokOpts[imask] |= iprio;
}

int SFKMatch::equalOrLowerPrio(uint8_t ioptl, uint8_t ioptr)
{
   // left side prio 1, right 3 means left has a LOWER prio
   if ((ioptl & TOKOPT_PRIOMASK) <= (ioptr & TOKOPT_PRIOMASK))
      return 1;
   return 0;
}

int SFKMatch::matchesSinglePoint(int imask, uint8_t *pSrcCur, uint8_t *pSrcMax,
   bool bLastRecord, int &rMatchLen, bool bStart, int iFrom, bool bLineStart)
{
   int itok=0,iminlen=0,imaxlen=0;
   uint8_t *aSkipMatch=0,iopt=0;

   itok    = aClFrom[imask];
   iopt    = aClTokOpts[imask];
   iminlen = aiClFromMinLen[imask];
   imaxlen = aiClFromMaxLen[imask];

   #ifdef SFKINT
   if (iopt & TOKOPT_REFLEN) { // lenref
      if ((iminlen = calcLength(imask, iopt)) < 0)
         return 9+sfkerr("invalid input length function (rc=%d)",iminlen);
      imaxlen = iminlen;
   }
   #endif // SFKINT

   uint8_t *pCurLit = 0, *pFlags = 0;
   int    iCurLit = 0;
 
   int iRemain = pSrcMax - pSrcCur;

   rMatchLen = 0;

   switch (itok)
   {
      case TokLiteral:
      {
         int ilitoff = 0;
         #ifdef SFK_LOR
         ilitoff = imaxlen;
         #endif // SFK_LOR
         pCurLit = aClData[imask]+ilitoff;
         if (!pCurLit)
            { sfkerr("int. #2144291"); return 19; }
         pFlags  = aClFlags[imask]; // or null, no ilitoff
         // Note: with [ortext] this is a combined length over all literals!
         iCurLit = imaxlen;
         #ifdef SFK_LOR
         if (aClOrInf[imask])
         {
            // char szBuf[100];
            // consider [ortext] parts
            int *pOrInf = aClOrInf[imask];
            // printf("# match: '%s' @ %s\n",dataAsTrace(pCurLit,iCurLit),pSrcCur);
            int iSubStr = 1; // ONE based!
            int nSubStr = pOrInf[0];
            int iMaxLen = iCurLit;
            int iCurOff = 0;
            int iCurMax = 0;
            int iCurLen = 0;
            int isubrc  = 5; // no match
            // printf("# match %d substr\n",nSubStr);
            while (iSubStr<=nSubStr && iCurOff<iMaxLen)
            {
               // get current substring
               iCurMax = pOrInf[iSubStr];
               iCurLen = iCurMax - iCurOff;
               // check for match.
               if (iCurLen <= iRemain) {
                  // printf("#  check #%d: off %d len %d %s @ %s\n",iSubStr,iCurOff,iCurLen,dataAsTrace(pCurLit+iCurOff,iCurLen),dataAsTrace(pSrcCur,strlen((char*)pSrcCur),szBuf,100));
                  if (!sfkmemcmp3(pSrcCur, pCurLit, iCurLen, bClUseCase, pFlags, iCurOff)) {
                     isubrc = 0;
                     break;
                  }
               }
               // goto next, if any
               iCurOff += iCurLen;
               iSubStr++;
            }
            if (isubrc)
               return isubrc;
            // printf("#  MATCH #%d: off %d len %d %s\n",iSubStr,iCurOff,iCurLen,dataAsTrace(pCurLit+iCurOff,iCurLen));
            rMatchLen = iCurLen;
            return 0;
         }
         else
         #endif // SFK_LOR
         {
            if (iCurLit > iRemain) {
               if (traceLevel() > 2) trace("match.miss%u:  literal: %s", iFrom, pCurLit);
               return 5;
            }
         }
         if (sfkmemcmp2(pSrcCur, pCurLit, iCurLit, bClUseCase, pFlags)) {
            if (traceLevel() > 2) trace("match.miss%u:  literal: %s", iFrom, pCurLit);
            return 6;
         }
         rMatchLen = iCurLit;
         return 0;
      }

      case TokEndOrEOL:
         if (pSrcCur >= pSrcMax) {
            if (bLastRecord) {
               trace("tokendoreol.1");
               rMatchLen = 0;
               return 0;
            }
            if (traceLevel() > 2) trace("match.miss%u:  end", iFrom);
            return 9;
         }
         if (imask+1 < iClFrom)
            return 9+sfkerr("lend must be last token");
         if (iRemain >= 2 && !memcmp(pSrcCur, "\r\n", 2)) {
            rMatchLen = 0;
            return 0;
         }
         if (iRemain >= 1 && pSrcCur[0] == '\n') {
            rMatchLen = 0;
            return 0;
         }
         if (iRemain >= 1 && pSrcCur[0] == '\r') {
            rMatchLen = 0;
            return 0;
         }
         if (traceLevel() > 2) trace("match.miss%u:  eol", iFrom);
         return 11;
         // fall through to EOL

      case TokEOL:
         // CRLF or LF or CR
         if (iRemain >= 2 && !memcmp(pSrcCur, "\r\n", 2)) {
            rMatchLen = 2;
            return 0;
         }
         if (iRemain >= 1 && pSrcCur[0] == '\n') {
            rMatchLen = 1;
            return 0;
         }
         if (iRemain >= 1 && pSrcCur[0] == '\r') {
            rMatchLen = 1;
            return 0;
         }
         if (traceLevel() > 2) trace("match.miss%u:  eol", iFrom);
         return 11;

      case TokByte:
      case TokClass:
         aSkipMatch = aClClass[imask];
         if (!aSkipMatch)
            { sfkerr("int. #2144281"); return 19; }
         if (iminlen > iRemain) {
            if (traceLevel() > 2) trace("match.miss%u:  char class at eod", iFrom);
            return 7;
         }
         if (iminlen==imaxlen) {
            if (aSkipMatch == aClSkipMatchBytes) {
               // pure [bytes] requires no cmp
            }
            else
            if (classcmp(imask,pSrcCur,aSkipMatch,iminlen)) {
               if (traceLevel() > 2) trace("match.miss%u:  char class fixed len", iFrom);
               return 7;
            }
            rMatchLen = iminlen;
         } else {
            if (!aSkipMatch[*pSrcCur]) {
               if (traceLevel() > 2) trace("match.miss%u:  #%u char class single 0x%02X %s", iFrom, imask, *pSrcCur, classAsTrace(aSkipMatch,40));
               return 8;
            }
            rMatchLen = 1;
         }
         return 0;

      case TokStartOr:
         if (bStart) {
            rMatchLen = 0;
         } else {
            aSkipMatch = aClClass[imask];
            if (!aSkipMatch)
               { sfkerr("int. #2144282"); return 19; }
            if (!aSkipMatch[*pSrcCur]) {
               if (traceLevel() > 2) trace("match.miss%u:  start or code", iFrom);
               return 8;
            }
            rMatchLen = 1;
         }
         return 0;

      case TokLStart:
         if (bStart || bLineStart) {
            rMatchLen = 0;
            return 0;
         }
         return 12;

      case TokEnd:
         if (pSrcCur >= pSrcMax) {
            if (bLastRecord) {
               trace("tokend.1 (%d)", iFrom);
               rMatchLen = 0;
               return 0;
            }
            if (traceLevel() > 2) trace("match.miss%u:  end", iFrom);
            return 9;
         }
         aSkipMatch = aClClass[imask];
         if (!aSkipMatch)
            { sfkerr("int. #2144283"); return 19; }
         if (aSkipMatch[*pSrcCur]) {
            trace("tokend.2");
            rMatchLen = 1;
            return 0;
         }
         if (traceLevel() > 2) trace("match.miss%u:  end", iFrom);
         return 10;
   }

   if (traceLevel() > 2) trace("match.miss%u:  unknown", iFrom);

   return 1;
}

void SFKMatch::updateBestMatch(int imask)
{
   if (imask+1 > iClBestMatch)
      iClBestMatch = imask+1;
}

// RC 0  : match
// RC >0 : nomatch
int SFKMatch::matches(uint8_t *pSrcData, int &rIOLength,
   int bStart, int *pIOLineStart, int bLastRecord,
   char *pSrcAttr)
{
   if (traceLevel() > 2)
      trace("match.enter: len=%d start=%d lastrec=%d", rIOLength, bStart, bLastRecord);

   // optim: first part must always be a literal or char class
   if (bStart == 0)
   {
      if (rIOLength > 0)
         if (!aClHeadMatch[*pSrcData])
            return 3;
      if (aClFrom[0] == TokStart)
         return 2;
   }

   bool bLineStart = *pIOLineStart;

   int iSrcMax = rIOLength;

   uint8_t *pSrcCur   = pSrcData;
   uint8_t *pSrcMax   = pSrcData + iSrcMax;
   uint8_t *pSrcOr    = 0;  // then, or restart point
   uint8_t *pSrcBest  = 0;  // match max pos for outlen calc
   uint8_t *pSrcKeep  = 0;  // start of [keep]
   char  *pAttrCur  = 0;

   int imask=0,istate=0,iormatch=0;
   int itok=0,iopt=0,iminlen=0,imaxlen=0;
   int iMatchLen=0;

   int iSkipRemain=0,iPart=-1;
   uint8_t *aSkipMatch=0;
 
   int iPrevRemain=0,iPrevPart=-1;
   uint8_t *aPrevSkipMatch=0;

   uint8_t *pCurLit = 0;
   int    iCurLit = 0;

   uint8_t uc = 0;

   bool bAnyStart = 0;

   // init lengths of parts to collect
   memset(aiClData, 0, iClFromTok * sizeof(int));

   /*
      foo[100*]bar
      foo*
   */
 
   while (1)
   {
      // --- end of OR group handling begin ---
      if (istate == 30) { // local match
         if (pSrcOr!=0 && aClFrom[imask]!=0 && (aClTokOpts[imask]&TOKOPT_OR)!=0) {
            // reached the next OR after a local match
            iormatch++;
         }
         else
         if (pSrcOr!=0 && aClFrom[imask]==0) {
            // reached end of command after a local match
            iormatch++;
         }
         istate = 0;
      }
      if (istate == 31) { // local mismatch
         if (!pSrcOr)
            return 1;
         // an or restart point exists.
         // is there any further OR?
         if (aClFrom[imask] != 0) {
            imask++;
            while (aClFrom[imask]!=0 && (aClTokOpts[imask]&TOKOPT_OR)==0)
               imask++;
         }
         istate = 0;
      }
      // --- end of OR group handling end ---

      if (traceLevel() > 1)
         trace("match.step : off=%d imask=%d tok=%d\n",
            (int)(pSrcCur-pSrcData), imask, aClFrom[imask]);

      if (aClFrom[imask] == 0) {
         // overall end of command. is it an overall match?
         if (pSrcOr!=0 && iormatch==0)
            return 1; // not a single OR group matched
         break;
      }

      if (pSrcCur >= pSrcMax) {
         // if a class reached EOD within an OR group
         if (pSrcOr) {
            // then we may continue with next OR group
            istate=31;
            pSrcCur=pSrcOr;
            continue;
         }
         if (    bLastRecord
             && (aClFrom[imask] == TokEnd || aClFrom[imask] == TokEndOrEOL)
            )
         {
            // zero length run for [eod] token
            trace("match.eodl : check endtok st=%d", istate);
            uc=0;
         }
         else {
            trace("match.eod  : #%u $%u %u last=%d\n",
               imask, istate, itok, bLastRecord
               );
            return 1;
         }
      }
      else {
         uc = pSrcCur[0];
      }
 
      if (istate == 0)  // current token not yet read
      {
         itok    = aClFrom[imask];
         iopt    = aClTokOpts[imask];
         iminlen = aiClFromMinLen[imask];
         imaxlen = aiClFromMaxLen[imask];

         if (iopt & TOKOPT_KEEP) { // e.g. [keep]\n-
            pSrcKeep = pSrcCur;
         }
         #ifdef SFKINT
         if (iopt & TOKOPT_REFLEN) { // lenref
            if ((iminlen = calcLength(imask, iopt)) < 0)
               return 9+sfkerr("invalid input length function (rc=%d)",iminlen);
            imaxlen = iminlen;
         }
         #endif // SFKINT

         if (iopt & TOKOPT_OR) {
            if (pSrcOr) {
               // OR restart position is used from 2nd OR
               pSrcCur = pSrcOr;
            } else {
               // OR restart position is set on first OR
               pSrcOr = pSrcCur;
            }
         }

         istate = 1;
      }
 
      if (istate == 1)  // continue current token
      {
         if (traceLevel() > 1)
            trace("match.exec : #%u $%u t%u o%u %02d.%04d %-30.30s   %03d\n",
               imask, istate, itok, iopt, iminlen, imaxlen,
               dataAsTrace(pSrcCur,mymin(20,pSrcMax-pSrcCur)),
               (int)(pSrcMax-pSrcCur)
               );

         // handle special tokens
         switch (itok)
         {
            case TokStart:
               if (!bStart)
                  return 1;
               updateBestMatch(imask);
               bStart = 0;
               istate = 0;
               imask++;
               continue;

            case TokLStart:
               if (!bStart && !*pIOLineStart)
                  return 1;
               updateBestMatch(imask);
               break; // fall through

            case TokStartOr:
               // oldlstart
               // first check for class match
               aSkipMatch = aClClass[imask];
               if (!aSkipMatch)
                  { sfkerr("int. #2144284"); return 19; }
               if (aSkipMatch[*pSrcCur])
                  break; // fall through
               // then check for data start
               if (!bStart)
                  return 1;
               updateBestMatch(imask);
               break; // fall through
         }
 
         // if (CURRENT token is varlength)
         // first check following token
         //    LIKE: *foo
         //    NOT : [start or byte of ...]foo
         //    NOT : [eol]foo
         if (   (iopt & TOKOPT_REFLEN) == 0
             && iminlen != imaxlen
            )
         {
            int bmnn=-1,bmcc=-1,bmnc=-1,iDummyLen=0;

            // NOTE: mind iMatchLen changes!

            // if there is a non zero minlen, execute current token until filled
            if (aiClData[imask] >= iminlen)
            {
               // if (NEXT token matches current point)
               if (   aClFrom[imask+1] != 0
                   && equalOrLowerPrio(iopt, aClTokOpts[imask+1]) != 0
                   && !(bmnc=matchesSinglePoint(imask+1, pSrcCur, pSrcMax, bLastRecord, iDummyLen, bStart, 3, bLineStart))
                  )
               {
                  // stop adding to current token,
                  // switch to NEXT token
                  trace("match.2next:  #%u t%d %u.%u bmcc=%d bmnc=%d bmnn=%d",
                     imask+1,aClFrom[imask+1],
                     aiClFromMinLen[imask+1],
                     aiClFromMaxLen[imask+1],
                     bmcc,bmnc,bmnn);
                  imask++;
                  istate=0;
                  continue;
               }
            }
            // trace("greedy: bmcc=%d bmnn=%d",bmcc,bmnn);
            // if (CURRENT token matches current point)
            if (bmcc==-1)
                bmcc = matchesSinglePoint(imask, pSrcCur, pSrcMax, bLastRecord, iMatchLen, bStart, 4, bLineStart);
            if (!bmcc)
            {
               // local match (varlen)
               updateBestMatch(imask);
               // collect current token
               pAttrCur = pSrcAttr ? pSrcAttr+(pSrcCur-pSrcData) : 0;
               if (iMatchLen > 0)
                  if (addData(imask, pSrcCur, iMatchLen, pAttrCur))
                     return 9;
               pSrcCur += iMatchLen;
               if (pSrcCur > pSrcBest)
                  pSrcBest = pSrcCur;
               // EOL must stop on first match, do not collect "\n\n".
               // StartOr must be finished now (zero or one byte).
               if (   itok == TokEOL || itok == TokEndOrEOL
                   || itok == TokStartOr || itok == TokLStart
                   || itok == TokEnd
                  )
               {
                  imask++;
                  istate=30;
                  continue;
               }
               // all other varlen patterns are greedy,
               // i.e. they collect as much data as possible.
               // if maxlen is reached, enforce switch to next token.
               // if EOD is reached, enforce switch to (EOD) token.
               if (pSrcCur >= pSrcMax || aiClData[imask] >= imaxlen) {
                  imask++;
                  istate=30;
               }
               continue;
            }
            // mismatch with current char class
            if (itok != TokClass)
               { istate=31; continue; }
            // with "[1.10 *]" check if min range was reached
            if (aiClData[imask] < iminlen)
               { istate=31; continue; }
            // with "[0.90 *]" it means to step to next mask
            imask++;
            istate = 0;
            continue;
         }
         // current token is FIXED length
         // if (CURRENT token matches current point)
         if (!matchesSinglePoint(imask, pSrcCur, pSrcMax, bLastRecord, iMatchLen, bStart, 5, bLineStart))
         {
            int iDataLen = iminlen;
            #ifdef SFK_LOR
            iDataLen = iMatchLen;
            #endif // SFK_LOR
            // local match (fixlen)
            updateBestMatch(imask);
            // collect current token data (whole length)
            pAttrCur = pSrcAttr ? pSrcAttr+(pSrcCur-pSrcData) : 0;
            if (addData(imask, pSrcCur, iDataLen, pAttrCur))
               return 9;
            // switch to NEXT token
            pSrcCur += iDataLen;
            if (pSrcCur > pSrcBest)
               pSrcBest = pSrcCur;
            imask++;
            istate=30;
            continue;
         }
         // else mismatch
         istate=31;
         continue;
      }
   }

   // apply [keep] after match
   if (pSrcKeep != 0) {
      pSrcCur = pSrcKeep;
      pSrcBest = pSrcCur;
   }

   if (pSrcCur > pSrcBest)
      pSrcBest = pSrcCur;

   rIOLength = pSrcBest - pSrcData;

   // for simple output rendering
   pClCurMatchAddr = pSrcData;
   iClCurMatchLen  = pSrcBest - pSrcData;

   // special case: /[start]/
   if (imask==1 && aClFrom[0]==TokStart) {
      trace("MATCH : /[start]/ at file start\n");
      *pIOLineStart = 1;
      return 0;
   }
   // special case: /[end]/
   if (imask==1 && aClFrom[0]==TokEnd) {
      trace("MATCH : /[end]/ at file end\n");
      *pIOLineStart = 1;
      return 0;
   }
   // special case: /[end]/
   if (imask==1 && aClFrom[0]==TokEndOrEOL) {
      trace("MATCH : /[lend]/ at file end\n");
      *pIOLineStart = 1;
      return 0;
   }

   // special case: [white][bytes of ...][xwhite] logically "matching"
   // however they are all empty, matching a zero range.
   if (rIOLength == 0) {
      trace("match.empty: return miss");
      return 1;
   }

   trace("MATCH : len=%d \"%s\"\n", rIOLength, dataAsTrace(pSrcData,mymin(32,rIOLength)));

   if (rIOLength < 1) {
      // should never occur as it causes endless loop
      sfkerr("SFKMatch internal error: zero length hit for \"%s\"\n", pszClFromText);
      return 99;
   }

   // on every match adapt line start state
   if (pSrcData[rIOLength-1] == '\n')
      *pIOLineStart = 1;
   else
   if (imask>0 && aClFrom[imask-1]==TokEndOrEOL)
      *pIOLineStart = 1;
   else
      *pIOLineStart = 0;

   return 0;
}

char *SFKMatch::outAttr( )
{
   return aClOutAttr;
}

int SFKMatch::viewOffset( )
{
   if (!bClAlloc)
      return 0;
   if (!aClFrom || !iClFrom || !aiClData)
      return 0;
   // oldlstart
   // if (aClFrom[0] == TokLStart)
   //   return aiClData[0];
   return 0;
}

void attrCopy(char *pDst, char *pSrc, int iLen)
{
   for (int i=0; i<iLen; i++)
   {
      if (pSrc[i] != 0)
         pDst[i] = pSrc[i];
      else
         pDst[i] = ' ';
   }
}

uint8_t *SFKMatch::renderOutput(int &rOutLength, int &rRC, int nFlags)
{
   if (!aClOutBuf)
      { sfkerr("missing call: provideBuffer"); return 0; }

   rOutLength = 0;

   int imask=0,istate=0;
   int itok=0,imaxlen=0;
   int ipart=0,iopt=0;

   uint8_t *pCurLit = 0;
   int    iCurLit = 0;
   char  *pCurAttr= 0;

   #ifdef SFKVAR
   char *pVarName  = 0; // current setvar name
   int   iVarStart = 0; // output data start index
   #endif

   uint8_t *pDstCur = aClOutBuf;
   uint8_t *pDstMax = aClOutBuf + iClOutSizeAlloc;

   iClOut = 0;

   while (aClTo[imask] != 0)
   {
      if (pDstCur >= pDstMax)
         { sfkerr("output overflow.1"); rRC=9; return 0; }

      itok    = aClTo[imask] & 0xFFU;
      imaxlen = aClTo[imask] >> 8;
      iopt    = aClOutOpts[imask];

      pCurLit  = 0;
      pCurAttr = 0;
      iCurLit  = 0;

      if (itok == TokLiteral) {
         pCurLit = aClToLit[imask];
         iCurLit = imaxlen;
         if (!pCurLit || !iCurLit)
            { rRC=9; sfkerr("internal #2137185"); return 0; }
         if (iCurLit < 0 || pDstCur+iCurLit > pDstMax) {
            sfkerr("render: invalid text%d length %d",imask+1,iCurLit);
            rRC=9; return 0;
         }
         trace("render: add text%d len=%02d: %s", imask+1, iCurLit, dataAsTrace(pCurLit, iCurLit));
      }
      #ifdef SFKVAR
      else
      if (itok == TokSetVar) {
         // start collecting data for a variable
         pVarName = (char*)aClToLit[imask];
         if (!pVarName)
            { rRC=9; sfkerr("internal #2166291"); return 0; }
         iVarStart = (int)(pDstCur - aClOutBuf);
      }
      else
      if (itok == TokEndVar) {
         // done collecting data, store as variable
         if (!pVarName)
           { rRC=9; sfkerr("internal #2166292"); return 0; }
         int iVarEnd = (int)(pDstCur - aClOutBuf);
         int iVarLen = iVarEnd - iVarStart;
         if ((nFlags & SFKMATCH_SETNOVAR) == 0)
            sfksetvar(pVarName, aClOutBuf+iVarStart, iVarLen);
         // strip variable text from output
         pDstCur = aClOutBuf + iVarStart;
         // cleanup
         pVarName = 0;
         iVarStart = 0;
      }
      else
      if (itok == TokGetVar) {
         char *pTmpVarName = (char*)aClToLit[imask];
         if (!pTmpVarName)
            { rRC=9; sfkerr("internal #2166291"); return 0; }
         pCurLit = sfkgetvar(pTmpVarName, &iCurLit);
         // NULL if no such var, and iCurLit stays 0
         if (pCurLit == 0)
            iCurLit = 0; // safety
      }
      #endif
      else
      if (itok == TokOutFN) {
         // similar to literal
         pCurLit = aClToLit[imask];
         iCurLit = imaxlen;
         if (!pCurLit || !iCurLit)
            { rRC=9; sfkerr("internal #2137189"); return 0; }
         // but replace literal by OutFN output
         int iIOLen = iCurLit;
         uint8_t *pOutData = 0;
         int    iDataLen = 0;
         int iSubRC = 0;
         if ((iSubRC = pClOutFN(2, (char*)pCurLit, &iIOLen, &pOutData, &iDataLen))) {
            sfkerr("render: error %d while rendering %s",iSubRC,(char*)pCurLit);
            rRC=9; return 0;
         }
         if (iDataLen>0 && pOutData==0) {
            sfkerr("render: null result while rendering %s",(char*)pCurLit);
            rRC=10; return 0;
         }
         // empty results are allowed
         pCurLit = pOutData;
         iCurLit = iDataLen;
      }
      else
      if (itok == TokPart) {
         if (imaxlen < 1) {
            sfkerr("render: invalid output token %d (value=%d)",imask,imaxlen);
            rRC=9; return 0;
         }
         ipart = imaxlen-1;
         // take part text as a literal
         if (ipart >= iClFromTok)
            { rRC=9; sfkerr("wrong part number: %d", imaxlen); return 0; }
         pCurLit = aClData[ipart];
         iCurLit = aiClData[ipart];
         if (!pCurLit)
            { rRC=9; sfkerr("no such part: %d", imaxlen); return 0; }
         if (iCurLit < 0 || pDstCur+iCurLit > pDstMax) {
            sfkerr("render: invalid part%d length %d",ipart+1,iCurLit);
            rRC=9; return 0;
         }
         if (bClAttr && aClAttr) {
            pCurAttr = aClAttr[ipart];
         }
         trace("render: add part%d len=%02d: %s", ipart+1, iCurLit, dataAsTrace(pCurLit, iCurLit));
      }
      else {
         sfkerr("int. #2138211");
         rRC=11;
         return 0;
      }

      if (pDstCur + iCurLit > pDstMax) {
         sfkerr("output overflow.2: %d %d", (int)iCurLit, (int)(pDstMax-pDstCur));
         rRC=9; return 0;
      }

      if (iCurLit > 0 && pCurLit == 0)
         { sfkerr("int. #2131211"); rRC=11; return 0; }

      if (iopt & 1)
      {
         // output option: strip \0 bytes
         int iDst=0;
         int iAttrOff=pDstCur-aClOutBuf;
         for (int iSrc=0; iSrc<iCurLit; iSrc++)
         {
            if (pCurLit[iSrc]!=0)
            {
               pDstCur[iDst] = pCurLit[iSrc];
               if (bClAttr && aClOutAttr) {
                  if (pCurAttr)
                     aClOutAttr[iAttrOff+iDst] = pCurAttr[iSrc];
                  else
                     aClOutAttr[iAttrOff+iDst] = ' ';
               }
               iDst++;
            }
         }
         iCurLit = iDst;
      }
      else
      {
         if (iCurLit > 0 && pCurLit != 0)
         {
            memcpy(pDstCur, pCurLit, iCurLit);

            // fix 1770: do this only if iCurLit > 0
            if (bClAttr && aClOutAttr) {
               if (pCurAttr) {
                  if (cClLitAttr) {
                     attrCopy(aClOutAttr+(pDstCur-aClOutBuf), pCurAttr, iCurLit);
                  } else {
                     memcpy(aClOutAttr+(pDstCur-aClOutBuf), pCurAttr, iCurLit);
                  }
               } else {
                  memset(aClOutAttr+(pDstCur-aClOutBuf), ' ', iCurLit);
               }
            }
         }
      }

      pDstCur += iCurLit;
      imask++;
   }

   if (!bClExtract && !bClXText && iClFrom>0 && (aClFrom[iClFrom-1] == TokEndOrEOL))
   {
      // replace-only forced lend insertion
      pCurLit = aClData[iClFrom-1];
      iCurLit = aiClData[iClFrom-1];
      if (iCurLit < 0 || pCurLit == 0 || pDstCur+iCurLit > pDstMax)
         { rRC=9; sfkerr("invalid lend on output rendering"); return 0; }
      if (bClAttr && aClAttr)
         pCurAttr = aClAttr[ipart];
      memcpy(pDstCur, pCurLit, iCurLit);
      if (bClAttr && aClOutAttr) {
         if (pCurAttr)
            memcpy(aClOutAttr+(pDstCur-aClOutBuf), pCurAttr, iCurLit);
         else
            memset(aClOutAttr+(pDstCur-aClOutBuf), ' ', iCurLit);
      }
      pDstCur += iCurLit;
   }

   *pDstCur = '\0'; // safety

   iClOut = pDstCur - aClOutBuf;

   rOutLength = iClOut;

   if (bClAttr && aClOutAttr) {
      aClOutAttr[iClOut] = '\0'; // safety
   }

   trace("    ==> len=%d \"%s\"", iClOut, dataAsTrace(aClOutBuf,mymin(32,iClOut)));

   return aClOutBuf;
}

