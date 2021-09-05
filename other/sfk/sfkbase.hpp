#ifndef _SFKBASE_HPP_
#define _SFKBASE_HPP_
/*
   swiss file knife base include
*/

// ========== core includes and operating system abstraction ==========

// enable LFS esp. on linux:
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64

#ifdef _WIN32
 #define WINFULL
#else
 #ifdef __APPLE__
  #ifndef MAC_OS_X
   #define MAC_OS_X
  #endif
 #endif
 #if defined(__sun) && defined(__SVR4)
  #define SOLARIS
 #endif
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <assert.h>
#include <time.h>
#include <math.h>
#include <sys/timeb.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>

#ifdef _WIN32
  #define FD_SETSIZE 300
  #ifdef _MSC_VER
   #ifndef SFKPRO
    #define SFKWINST
   #endif
   #include <comdef.h>
   #include <ShlObj.h>
   #include <Shlwapi.h>
  #endif
  #include <windows.h>
  #ifndef _MSC_VER
   #include <ws2tcpip.h>
  #endif
  #include <sys/timeb.h>
  #include <time.h>
  #include <process.h>
  #define getpid() _getpid()
  #include <errno.h>
  #include <direct.h>
  #include <signal.h>
  #include <io.h>
  #ifndef socklen_t
   #define socklen_t int
  #endif
  #ifndef SD_RECEIVE
   #define SD_RECEIVE 0x00
   #define SD_SEND    0x01
   #define SD_BOTH    0x02
  #endif
  #ifndef SHUT_RD
   #define  SHUT_RD   SD_RECEIVE
   #define  SHUT_WR   SD_SEND
   #define  SHUT_RDWR SD_BOTH
  #endif
  #define MSG_NOSIGNAL 0
  #define vsnprintf _vsnprintf
  #define snprintf  _snprintf
  // FILE_ATTRIBUTE_READONLY    0x00000001
  // FILE_ATTRIBUTE_HIDDEN      0x00000002
  // FILE_ATTRIBUTE_SYSTEM      0x00000004
  // FILE_ATTRIBUTE_DIRECTORY   0x00000010
  // FILE_ATTRIBUTE_ARCHIVE     0x00000020
  // FILE_ATTRIBUTE_NORMAL      0x00000080
  // FILE_ATTRIBUTE_TEMPORARY   0x00000100
  #define WINFILE_ATTRIB_MASK   0x0000001F
#else
  #include <unistd.h>
  #include <dirent.h>
  #include <sys/stat.h>
  #include <netinet/in.h>
  #include <sys/socket.h>
  #include <sys/ioctl.h>
  #include <sys/time.h>
  #include <arpa/inet.h>
  #include <netdb.h>
  #include <errno.h>
  #include <sched.h>
  #include <signal.h>
  #include <sys/statvfs.h>
  #include <utime.h>
  #include <pthread.h>
  #include <pwd.h>
  #include <ifaddrs.h>
  #include <fcntl.h>
  #ifndef MAC_OS_X
   #define SFK_NATIVE_LINUX
   #include <wait.h>
   #include <net/if_arp.h>
   #include <linux/sockios.h>
  #endif
  #ifndef  _S_IFDIR
   #define _S_IFDIR 0040000 // = 0x4000
  #endif
  #ifndef  _S_IREAD
   #ifdef S_IREAD
    #define _S_IREAD  S_IREAD
    #define _S_IWRITE S_IWRITE
   #else
    #define _S_IREAD  __S_IREAD  // by owner
   #define  _S_IWRITE __S_IWRITE // by owner
   #endif
  #endif
  typedef int SOCKET;
  #define INVALID_SOCKET -1
  #define SOCKET_ERROR   -1
  #define ioctlsocket ioctl
  #define WSAEWOULDBLOCK EWOULDBLOCK
  #ifndef MSG_NOSIGNAL
   #define MSG_NOSIGNAL 0
  #endif
#endif

// - - - - - basic types and tracing - - - - -

#ifdef MAC_OS_X
 #define fpos64_t  fpos_t
 #define fgetpos64 fgetpos
 #define fsetpos64 fsetpos
 #define statvfs64 statvfs
 #define stat64    stat
 #define __dev_t   dev_t
#endif // MAC_OS_X

#ifdef SOLARIS
 #define __dev_t   dev_t
 // #define FIONBIO   0
#endif

#define uchar unsigned char
#define ushort unsigned short
#define uint  unsigned int
#define ulong unsigned long
#define cchar const char
#define str(x) (char*)x

#ifdef __GNUC__
   #if __GNUC__ < 8
      #define bool unsigned char
   #endif
   // else use predefined type
#else
   #define bool unsigned char
#endif

#define mclear(x) memset(&x, 0, sizeof(x))

#ifdef CALLBACK_TRACING
 #define mtklog  cbtrace
 #define mtkerr  cbtrace
 #define mtkwarn cbtrace
 #define mtkdump
 #define _
 #define __
#else
 #ifdef WITH_TRACING
  #include "mtk/mtktrace.hpp"
  #ifdef MTKTRACE_CODE
   #include "mtk/mtktrace.cpp"
  #endif
  #define _  mtklog(("[sfk %d]",__LINE__));
  #define __ MTKBlock tmp983452(__FILE__,__LINE__,"");tmp983452.dummy();
 #else
  #define mtklog(x)
  #define mtkerr(x)
  #define mtkwarn(x)
  #define mtkdump
  #define _
  #define __
 #endif
#endif

extern const char  glblPathChar    ;
extern const char  glblWrongPChar  ;
extern const char *glblPathStr     ;
extern const char *glblAddWildCard ;
extern const char *glblDotSlash    ;
extern       char  glblNotChar     ;
extern       char  glblRunChar     ;
extern       char  glblWildChar    ;

#ifdef _WIN32
 #define SFK_FILT_NOT1 "-!"
 #define SFK_FILT_NOT2 "-ls!"
 #define SFK_FILT_NOT3 "-le!"
 #define EXE_EXT ".exe"
 #define SFK_SETENV_CMD "set"
#else
 #define SFK_FILT_NOT1 "-:"
 #define SFK_FILT_NOT2 "-ls:"
 #define SFK_FILT_NOT3 "-le:"
 #define EXE_EXT ""
 #define SFK_SETENV_CMD "export"
#endif

#define mymin(a,b) ((a<b)?(a):(b))
#define mymax(a,b) ((a>b)?(a):(b))

// - - - - - 64 bit abstractions - - - - -

#ifdef WINFULL
 #if _MSC_VER >= 1310
  #define SFK_W64
 #endif
 #if _MSC_VER >= 1900 // visual c++ 14.0
  #define SFK_NO_MEMTRACE
  #define SFK_VC14
  #define sfkfinddata64_t _wfinddata64_t
 #else
  #define sfkfinddata64_t __wfinddata64_t
 #endif
#endif

#ifdef _WIN32
 typedef __int64 num;
 typedef unsigned __int64 unum;
 #ifdef SFK_W64
  typedef __time64_t mytime_t;
  #define mymktime _mktime64
  #define mytime _time64
 #else
  typedef time_t mytime_t;
  #define mymktime mktime
  #define mytime time
 #endif
#else
 typedef long long num;
 typedef unsigned long long unum;
 typedef time_t mytime_t;
 #define mymktime mktime
 #define mytime time
#endif

extern struct tm *mylocaltime(mytime_t *ptime);
extern struct tm *mygmtime(mytime_t *ptime);

extern char *numtostr(num n, int nDigits, char *pszBuf, int nRadix);
extern char *numtoa_blank(num n, int nDigits=12);
extern char *numtoa(num n, int nDigits, char *pszBuf);
extern char *numtohex(num n, int nDigits, char *pszBuf);
extern num   atonum(char *psz);
extern num   myatonum(char *psz);
extern mytime_t getSystemTime();
extern int shrinkFormTextBlock(char *psz, int &rLen, bool bstrict, bool xchars=0, uchar **ppFlags=0);

#ifndef SIG_UNBLOCK
   #define SIG_UNBLOCK 2
#endif

#ifndef SIGALRM
   #define SIGALRM 14
#endif

#ifndef sigset_t
   #define sigset_t int
#endif

#ifndef sigemptyset
   #define sigemptyset(sig)
#endif

#ifndef sigaddset
   #define sigaddset( set, sig)
#endif

#ifndef sigprocmask
   #define sigprocmask(a, b, c)
#endif

// ========== end core includes and operating system abstraction ==========

#define SFK18

#ifndef SFKNOVFILE
 #define VFILEBASE
 #define VFILENET
 // #define USE_WEBCONCACHE
#endif // SFKNOVFILE

#if defined(SFKPRO)
 #ifndef USE_SFK_BASE
  #define SFKXDXE // pro compile
 #endif
#else
 #ifndef USE_SFK_BASE
  #if defined(SFKXD) // base+xd
   #define SFKXDXE
  #else
   #define SFKOSE    // default
  #endif
 #endif // USE_SFK_BASE
#endif // defined(SFKPRO)

#define WITH_CASE_XNN
#define SFKDEEPZIP   // sfk175

#ifndef SFKNOPACK
 #define SFKPACK     // sfk191
 #define SFKOFFICE   // sfk194
#endif

#ifdef _WIN32
 #define SFK_UNAME   // sfk190
#endif

int isDir(char *pszName);
cchar *sfkLastError();

#define MAX_ABBUF_SIZE 100000
#define MAX_LINE_LEN     4096

class Coi;
extern int (*pGlblJamCheckCallBack)(char *pszFilename);
extern int (*pGlblJamFileCallBack)(char *pszFilename, num &rLines, num &rBytes);
extern int (*pGlblJamLineCallBack)(char *pszLine, int nLineLen, bool bAddLF);
extern int (*pGlblJamStatCallBack)(Coi *pCoiOrNull, uint nFiles, uint nLines, uint nMBytes, uint nSkipped, char *pszSkipInfo);
extern int (*pGlblShowDataCallBack)(char *pszLine, int nLineLen);
extern int bGlblPassThroughSnap;

char *findPathLocation(cchar *pszCmd, bool bExcludeWorkDir=0);
extern int fileExists(char *pszFileName, bool bOrDir=0);

#define strcopy(dst,src) mystrcopy(dst,src,sizeof(dst)-10)
void  mystrcopy      (char *pszDst, cchar *pszSrc, int nMaxDst);
char *mystrstri      (char *phay, cchar *ppat);
int  mystrstrip      (char *pszHayStack, cchar *pszNeedle, int *lpAtPosition);
char *mystrrstr      (char *psrc, cchar *ppat);
char *mystrristr     (char *psrc, cchar *ppat);
int  mystrncmp      (char *psz1, cchar *psz2, int nLen, bool bCase=0);
int  mystricmp      (char *psz1, cchar *psz2);
int  mystrnicmp     (char *psz1, cchar *psz2, int nLen);
bool  strBegins      (char *pszStr, cchar *pszPat);
bool  striBegins     (char *pszStr, cchar *pszPat);
bool  strEnds        (char *pszStr, cchar *pszPat);
bool  striEnds       (char *pszStr, cchar *pszPat);
void  trimCR         (char *pszBuf);
void  removeCRLF     (char *pszBuf);
bool  sfkisalpha     (uchar uc);
bool  sfkisalnum     (uchar uc);
bool  sfkisprint     (uchar uc);
void  myrtrim        (char *pszBuf);
void  skipToWhite    (char **pp);
void  skipWhite      (char **pp);

char *loadFile       (char *pszFile, bool bquiet=0);
num   getFileSize    (char *pszName);
num   getCurrentTime ( );
num   atonum         (char *psz);   // decimal only
num   myatonum       (char *psz);   // with 0x support
char *numtoa         (num n, int nDigits=1, char *pszBuf=0);
char *numtohex       (num n, int nDigits=1, char *pszBuf=0);
int  timeFromString  (char *psz, num &nRetTime, bool bsilent=0, bool bUTC=0);
void  doSleep        (int nmsec);
void  doYield        ( );
uchar *loadBinaryFile(char *pszFile, num &rnFileSize);
uchar *loadBinaryFlex(char *pszFile, num &rnFileSize);
bool  infoAllowed    ( );
struct hostent *sfkhostbyname(const char *pstr, bool bsilent=0);
int   setaddr(struct sockaddr_in *paddr, char *pszHostOrIPPart, int iflags=0);

class IOStatusPhase {
public:
      IOStatusPhase  (cchar *pinfo);
     ~IOStatusPhase  ( );
};

void  resetIOStatus  ( );
num   countIOBytes   (num nbytes);
char  *getIOStatus   (num &nagemsec, num &nbytes, num &nmaxbytes);
// returns NULL if no status is set

class CharAutoDel {
public:
      CharAutoDel (char *p) { pClPtr = p; }
     ~CharAutoDel ( )       { if (pClPtr) delete [] pClPtr; }
     void deleteNow ( )     { if (pClPtr) delete [] pClPtr; pClPtr = 0; }
private:
      char *pClPtr;
};

class UCharAutoDel {
public:
      UCharAutoDel (uchar *p) { pClPtr = p; }
     ~UCharAutoDel ( )        { if (pClPtr) delete [] pClPtr; }
      void deleteNow ( )      { if (pClPtr) delete [] pClPtr; pClPtr = 0; }
private:
      uchar *pClPtr;
};

class CharAutoDelPP {
public:
      CharAutoDelPP (char **pp) { ppClPtr = pp; }
     ~CharAutoDelPP ( )         { if (*ppClPtr) delete [] *ppClPtr; }
private:
      char **ppClPtr;
};

class AutoRestoreInt {
public:
      AutoRestoreInt (int *pInt) { pClPtr = pInt; iClVal = *pInt; }
     ~AutoRestoreInt ( )         { *pClPtr = iClVal; }
private:
      int *pClPtr;
      int  iClVal;
};

// max length of sfk (internal) filenames and URL's
#define SFK_MAX_PATH 1024

#ifdef  MAX_PATH
 #undef MAX_PATH
#endif
#define MAX_PATH error_use_sfk_max_path

#ifdef  PATH_MAX
 #undef PATH_MAX
#endif
#define PATH_MAX error_use_sfk_max_path

#ifdef VFILEBASE
extern void  setDiskCacheActive(bool b);
extern bool  getDiskCacheActive( );
extern void  setDiskCachePath(char *p);
extern char *getDiskCachePath( );
#endif // VFILEBASE

extern void myclosesocket(SOCKET hsock, bool bread=1, bool bwrite=1);
bool userInterrupt   (bool bSilent=0, bool bWaitForRelease=0);

// some linux/mac sys\param.h define that:
#ifdef isset
 #undef isset
#endif

typedef unsigned uint32_t;

class SFKMD5
{
public:
    SFKMD5  ( );
   ~SFKMD5  ( );

   void   update  (uchar *pData, uint32_t nLen);
   uchar *digest  ( );
   void   reset   ( );

private:
   void   update     (uchar c);
   void   transform  ( );

   uint32_t nClCntHigh, nClCntLow;
   uchar    *pClBuf;
   uint32_t nClBufLen,nClBufCnt;
   uchar    *pClDig;
   uint32_t nClDigLen;
   uchar    aClBuf[100];
   uchar    aClDig[100];
   uint32_t alClSta[4];
   uint32_t alClBuf[16];
   uint32_t nClCRC;
   bool     bClDigDone;
};

// map of managed keys and NOT MANAGED values.
class KeyMap {
public:
      KeyMap   ( );
     ~KeyMap   ( );

   void  reset ( );
   // removes all entries. keys are deleted.
   // values are not deleted, as their type is unknown.

   int  put   (char *pkey, void *pvalue=0);
   // set a key, or put a value under a key.
   // the key is copied. pvalue is not copied.
   // if the key exists already, old pvalue is replaced.
   // rc 0:done >0:error.

   void  *get  (char *pkey, int *poutidx=0);
   // if a value was stored then it is returned.
   // if not, null is returned, no matter if the key is set.
   // if poutidx is given, it returns the nearest comparison index,
   // even if no direct hit for the key was found. this index
   // however must be used with care, as it can be < 0 or >= size.

   bool  isset (char *pkey);
   // tell if the key was ever put (with or without value).
   // rc: 0:not_set 1:is_set.

   void  setcase(bool bYesNo);
   // default for key comparison is CASE SENSITIVE.
   // use setcase(0) to select CASE INSENSITIVE.

   void  setreverse(bool bYesNo);
   // toggle reverse sorting order.

   int  remove(char *pkey);
   // remove entry with that key, if any.
   // rc: 0:done 1:no_such_key >=5:error.

   void *iget  (int nindex, char **ppkey=0);
   // walk through map entries, sorted by the key.
   // nindex must be 0 ... size()-1.
   // returns value, if any.
   // if key ptr is provided, it is set.

   int  put   (num nkey, void *pvalue=0);
   void *get   (num nkey);
   bool  isset (num nkey);
   int  remove(num nkey);
   void *iget  (int nindex, num *pkey);

   int  size  ( );
   // number of entries in KeyMap.

protected:
   void  wipe     ( );
   int  expand   (int n);
   int   bfind    (char *pkey, int &rindex);
   int  remove   (int nindex);

   int  nClArrayAlloc;
   int  nClArrayUsed;
   char  **apClKey;
   void  **apClVal;

   bool  bClCase;
   bool  bClRev;
};

// map of MANAGED string values.
class StringMap : public KeyMap {
public:
      StringMap   ( );
     ~StringMap   ( );

   void  reset    ( );

   int  put      (char *pkey, char *pvalue);
   // value is COPIED and MANAGED by StringMap.
   // also accepts NULL values, if you want to use
   // only isset() instead of get.

   char  *get     (char *pkey, char *pszOptDefault=0);
   char  *iget    (int nindex, char **ppkey);
   int   remove  (char *pkey);

   int  put      (num nkey, char *pvalue);
   char *get      (num nkey);
   char *iget     (int nindex, num *pkey);
   int  remove   (num nkey);
};

// map of MANAGED strings with attributes.
class AttribStringMap : public StringMap {
public:
      AttribStringMap   ( );
     ~AttribStringMap   ( );

   int  put      (char *pkey, char *ptext, char *pattr);
   char  *get     (char *pkey, char **ppattr);
   char  *iget    (int nindex, char **ppkey, char **ppattr);

   int  put      (num nkey, char *ptext, char *pattr);
   char *get      (num nkey, char **ppattr);
   char *iget     (int nindex, num *pkey, char **ppattr);

private:
   char *mixdup   (char *ptext, char *pattr);
   int  demix    (char *pmixed, char **pptext, char **ppattr);
};

class LongTable {
public:
   LongTable            ( );
  ~LongTable            ( );
   int addEntry        (int nValue, int nAtPos=-1);
   int updateEntry     (int nValue, int nIndex);
   int numberOfEntries ( );
   int getEntry        (int iIndex, int nTraceLine);
   void resetEntries    ( );
private:
   int expand          (int nSoMuch);
   int nClArraySize;
   int nClArrayUsed;
   int *pClArray;
};

class StringTable {
friend class Array;
public:
   StringTable          ( );
  ~StringTable          ( );
   int addEntry        (char *psz, int nAtPos=-1, char **ppCopy=0);
   int removeEntry     (int nAtPos);
   int numberOfEntries ( );
   char *getEntry       (int iIndex, int nTraceLine);
   int  setEntry       (int iIndex, char *psz);
   void resetEntries    ( );
   bool isSet           (int iIndex);
   void dump            (int nIndent=0);
   int find            (char *psz); // out: index, or -1
private:
   int addEntryPrefixed(char *psz, char cPrefix);
   int setEntryPrefixed(int iIndex, char *psz, char cPrefix);
   int expand          (int nSoMuch);
   int nClArraySize;
   int nClArrayUsed;
   char **apClArray;
};

class NumTable {
public:
   NumTable            ( );
  ~NumTable            ( );
   int addEntry        (num nValue, int nAtPos=-1);
   int updateEntry     (num nValue, int nAtPos);
   int numberOfEntries ( );
   num  getEntry        (int iIndex, int nTraceLine);
   void resetEntries    ( );
private:
   int expand          (int nSoMuch);
   int nClArraySize;
   int nClArrayUsed;
   num  *pClArray;
};

class Array {
public:
   Array                (const char *pszID); // one-dimensional by default
  ~Array                ( );
   int addString       (char *psz);   // to current row (0 by default)
   int addString       (int lRow, char *psz);
   int setString       (int lRow, int iIndex, char *psz);
   char *getString      (int iIndex); // use isStringSet() before
   char *getString      (int lRow, int iIndex);
   int addLong         (int nValue, int nTraceLine); // to current row (0 by default)
   int addLong         (int lRow, int nValue, int nTraceLine);
   int getLong         (int iIndex); // use isLongSet() before
   int getLong         (int lRow, int iIndex, int nTraceLine);
   int setLong         (int lRow, int iIndex, int nValue, int nTraceLine);
   int numberOfEntries ( );           // in current row (0 by default)
   int numberOfEntries (int lRow);
   bool isLongSet       (int lRow, int iIndex); // index and type test
   bool isStringSet     (int iIndex); // index and type test
   bool isStringSet     (int lRow, int iIndex);
   int addRow          (int nTraceLine); // add empty row, set as current
   int setRow          (int iCurRow, int nTraceLine);// set current row
   bool hasRow          (int iRow);   // tell if row exists
   void reset           ( );           // removes all entries
   void dump            ( );
   int addNull         (int lRow);
private:
   bool isSet           (int iIndex); // in current row (0 by default)
   int ensureBase      ( );  // make sure at least one row exists
   int expand          (int nSoMuch);
   int nClRowsSize;
   int nClRowsUsed;
   StringTable **apClRows;
   int nClCurRow;
   const char *pszClID;
};

class FileSet {
public:
   FileSet  ( );
  ~FileSet  ( );
   int  beginLayer     (bool bWithEmptyCommand, int nTraceLine);
   int  addRootDir     (char *pszName, int nTraceLine, bool bNoCmdFillup,
                        bool bAutoUseArc=false); // sfk193 addrootdir
   int  addDirMask     (char *pszName);
   int  addDirCommand  (int);
   int  addFileMask    (char *pszName);
   int  autoCompleteFileMasks   (int nWhat);
   void  setBaseLayer   ( );
   void  reset          ( );
   void  shutdown       ( );
   bool  hasRoot        (int iIndex);
   char* setCurrentRoot (int iIndex);
   bool  changeFirstRoot(char *pszNewRoot);
   char* getCurrentRoot ( );
   int   changeSingleRoot(char *pszNew); // for webserv
   char* root           (bool braw=0); // like above, but returns "" if none, with braw: 0 if none
   int  numberOfRootDirs ( );
   Array &rootDirs      ( ) { return clRootDirs; }
   Array &dirMasks      ( ) { return clDirMasks; }
   Array &fileMasks     ( ) { return clFileMasks; }
   bool  anyRootAdded   ( );
   bool  anyFileMasks   ( );  // that are non-"*"
   char* firstFileMask  ( );  // of current root
   void  dump           ( );
   void  info           (void (*pout)(int nrectype, char *pline));
   int  getDirCommand  ( );  // of current root
   int  checkConsistency  ( );
   char *currentInfoLine(int iLine);
// private:
   int  ensureBase     (int nTraceLine);
   void  resetAddFlags  ( ); // per layer
   Array clRootDirs;    // row 0: names/str, row 1: command/int, row 2: layer/int
   Array clDirMasks;    // row == layer
   Array clFileMasks;   // row == layer
   int   nClCurDir;
   int   nClCurLayer;
   char  *pClLineBuf;
   int   bClGotAllMask;
   int   bClGotPosFile;
   int   bClGotNegFile;
};

class StringPipe
{
public:
      StringPipe  ( );
      char *read  (char **ppAttr=0);  // returns 0 on EOD
      void  resetPipe ( );
      bool  eod   ( );
      void  dump  (cchar *pszTitle);
      int  numberOfEntries ( ) { return clText.numberOfEntries(); }
      char *getEntry        (int nIndex, int nLine, char **pAttr=0);
      void  resetEntries    ( );
      int  addEntry        (char *psz, char *pAttr);
      int  setEntry        (int iIndex, char *psz, char *pAttr);
private:
   StringTable clText;
   StringTable clAttr;
   int nReadIndex;
};

class FileList {
public:
   FileList       ( );
  ~FileList       ( );
   int  addFile        (char *pszAbsName, char *pszRoot, num nTimeStamp, num nSize, char cSortedBy=0);
   int  checkAndMark   (char *pszName, num nSize);
   void  reset          ( );
   StringTable clNames;
   StringTable clRoots;
   NumTable    clTimes;
   NumTable    clSizes;
};

#ifdef _WIN32
 #ifdef SFK_W64
  typedef __finddata64_t SFKFindData;
 #else
  typedef _finddata_t SFKFindData;
 #endif
#else
struct SFKFindData 
{
   char *name;
   int   attrib;
   num   time_write;
   num   time_create;
   num   size;
   bool  islink;
   uint rawmode; // for tracing
   uint rawtype; // for tracing
   uint rawnlnk; // link count
   num   ninode;     // under linux
   __dev_t ostdev;   // under linux
   bool  bhavenode;  // under linux
};
#endif

class CoiData;

// the caching object identifer (Coi) represents
// a file, a directory, or an url to a remote object.
class Coi
{
public:
    Coi  (char *pszName, char *pszRootDir);
    Coi  (int iFromInfo);  // any number, to avoid unwanted tmp objects
   ~Coi  ( );

    // create deep copy, however containing only
    // the lightweight data like filename.
    Coi  *copy( );

   char  *name( );         // must be set
   char  *relName( );      // without any path portion
   char  *rootRelName( );  // returns full name if no root
   // in case of zip entries, may return subdir/thefile.txt
   char  *root(bool braw=0);  // "" if none, with braw: 0 if none
   char  *ref (bool braw=0);  // "" if none, with braw: 0 if none

   #ifdef WINFULL
   bool    haswname( );
   ushort *wname( );
   int     setwname(ushort *p);
   #endif

   #ifdef VFILEBASE
   char  *orgName( );      // same as name() except on redirects
   bool   wasRedirected( );
   #endif // VFILEBASE

   int  status   ( );
   // 0:yet_unknown 1:ok 9:fails_to_read

   // Coi's must have an initial filename,
   // but it can be changed later by this:
   int  setName  (char *pszName, char *pszOptRootDir=0);
   // if rootdir is not given, the old one is kept.

   bool hasName      ( );
   bool hasBadName   ( );

   void  setIsDir    (bool bYesNo); // sets anydir status
   bool  isAnyDir    (int ilevel=0);
   bool  isTravelDir (bool bTreatOfficeAsFile=0);
   bool  isDirLink   ( );

   int  setRef   (char *pszName);

   bool  hasSize  ( );
   bool  hasTime  ( );
   bool  hasAttr  ( );

   num   getSize  ( );
   num   getTime  ( );
   uint  getAttr  ( );
   // bits 2,1,0  : rwx other
   // bits 5,4,3  : rwx group
   // bits 8,7,6  : rwx user
   // bits 11,10,9: uid, gid, sticky
   // bits 12...15: linux file types
   // ------------
   // bit  30     : sfk: from linux
   // bit  31     : sfk: attributes are valid

   void  setSize  (num nSize );
   void  setTime  (num nMTime, num nCTime = 0); // just in memory

   int   setFileTime (num nMTime); // on disk

   int   writeAttr(uint nattr, bool bFullPreserve); // set and change on disk
   // RC 0: OK, else error
   // Changes only bits 0..11 but not the file type.
   // By default conforms to umask under linux.
   // With bFullPreserve, umask is ignored and all attribs are written.

   bool  isWriteable ( );

   void  fillFrom (void *pfdat); // SFKFindData ptr

   // available after fillFrom() only:
   bool  isHidden ( );
   bool  isLink   ( );

   // extra string outside coi definition:
   int  setExtStr   (char *psz);   // is copied
   char  *getExtStr  ( );           // null if none

   // check if coi is an existing file
   bool  existsFile  (bool bOrDir=0, int *pIsDir=0);

   // data I/O functions:
   bool   isFileOpen ( );
   int    open       (cchar *pmode); // like fopen, but RC 0 == OK
   // supported pmodes: "rb", "r", "r+b", "wb"
   // with "r", reading stops as soon as binary is detected.
   size_t read       (void *pbuf, size_t nbufsize);
   size_t readRaw    (void *pbuf, size_t nbufsize);
   void   close      ( );
   int    remove     ( );
   int    closeAndRemove   ( );

   int    seek       (num nOffset, int nOrigin);
   // rc0:ok >=0:failed to seek

   size_t write      (uchar *pbuf, size_t nbytes);
   // guarantees to write nbytes (incl. workaround
   // for windows network file writing bug).
   // rc: number of bytes written
   // in case of error, rc != nbytes

   int writeLine     (char *psz);
   // auto appends LF or CRLF depending on mode used in open()

   // heuristic check by file header if it's binary.
   // status can also be set from external.
   void   setBinaryFile (bool bYesNo);
   bool   isBinaryFile  ( );
   uchar  isUTF16       ( ); // 0x00==none 0xFE==le 0xEF==be
   bool   isSnapFile    ( );
   void   probeFile     ( ); // read file header if not done yet

   // readLine alloc's another I/O buffer on demand:
   int   readLine   (char *pszOutBuf, int nOutBufLen);

   int   renameto   (char *pszDst);

   // directory and archive processing:
   int   openDir     (int ilevel=0);  // prep for nextEntry()
   Coi   *nextEntry  ( );  // result owned by CALLER.
   void   closeDir   ( );  // required after processing.
   bool   isDirOpen  ( );

   #ifndef VFILEZIP
   int  isZipSubEntry  ( )   { return 0; }
   bool   isTravelZip   (int iTraceFrom, bool braw=0) { return 0; }
   void   setArc        (bool bIsArchive) { }
   bool   isKnownArc    ( )   { return 0; }
   #endif

   #ifdef SFKPACK
   bool   isOffice            (int iTraceFrom, bool bIgnoreOfficeMode=0);
   char  *officeSubName       ( );
   int    isOfficeSubEntry    ( );
   int    rawLoadOfficeDir    ( );
   Coi   *rawNextOfficeEntry  ( );
   void   rawCloseOfficeDir   ( );
   int    loadOfficeSubFile   (cchar *pszFromInfo);
   void   stripOfficeName     ( );
   #endif // SFKPACK

   #ifdef VFILEBASE

   int   rawLoadDir (int ilevel=0);

   Coi   *getElementByAbsName (char *pabs); // result is NOT locked

   bool  isNet    ( );  // ftp OR http
   bool  isFtp    ( );
   bool  isHttp   (char *pszOptURL=0);

   // if it's zip, http or ftp, then it's virtual
   bool  isVirtual(bool bWithRootZips=0);

   num   getUsedBytes   ( );  // info for the cache

   int    prefetch      (bool bLoadNonArcBinaries, num nFogSizeLimit, num nHardSizeLimit);

   // direct query of http header fields, if any given
   StringMap &headers     ( );            // creates on demand
   char      *header      (cchar *pname);  // returns NULL or the value

   static char *cacheName (char *pnamein, char *pbufin, int nbufsize, int *prDirPartLen=0);
   // nbufsize should be >= SFK_MAX_PATH

   #endif // VFILEBASE

   // reference counting:
   int  incref   (cchar *pTraceFrom); // increment refcnt
   int  decref   ( );  // decrement refcnt
   int  refcnt   ( );  // current no. of refs

   static bool bClDebug;

   bool   rawIsDir      ( );  // native filesystem dir

   int   getContent    (uchar **ppdata, num &rnSize);
   // ppdata is MANAGED BY COI! i.e. as soon as Coi
   // is deleted, returned data is deleted as well.
   // rc >0: unable to access file content.

   void   setContent    (uchar *pdata, num nsize, num ntime=0);
   // releases old content, if any.

   int   releaseContent( );
   // after getContent(), tell that data buffer can be freed.

   cchar  *lasterr      ( );

   #ifndef _WIN32
   // linux only:
   bool   haveNode      ( );
   num    getNode       ( );  // not always unique
   bool   haveFileID    ( );  // same as haveNode
   char  *getFileID     ( );  // built from node and device
   #endif

   // if status()==0, can call this:
   int  readStat        (char cFromInfo);
   
   int  getOpenElapsedTime  ( );  // elapsed msec since open(), or 0

   int  setKeepTime     (Coi *pSrc);

   // how much bytes of a file should be read to detect binary
   static int iBinaryCheckSize;

   // internal
   static int writeAttrRaw(char *pszFile, uint nattr, bool bFullPreserve, bool bVerbose);
   static int forceWriteable(char *pszFile);

private:
   // nextEntry() does additional checks, this does none:
   Coi   *nextEntryRaw  ( );  // result owned by CALLER.

   // native file system
   int   rawOpenDir    ( );  // prep for nextEntry()
   Coi   *rawNextEntry  ( );  // result owned by CALLER.
   void   rawCloseDir   ( );  // required after processing.

public:

   #ifdef VFILEBASE

   int   preload       (cchar *pszFromInfo, bool bsilent, int iStopMode, bool bfile=0);
   int   preload       (cchar *pszFromInfo, uchar **ppout, num &rsize, int iStopMode);
   int   provideInput  (cchar *pszFromInfo, bool bsilent=0);
   int   loadOwnFileRaw(num nmaxsize, uchar **ppout, num &rsize);
   int   preloadFromWeb( );

   // ftp folders and files
   bool   rawIsFtpDir    ( );
   //     rawLoadDir     ( )  // is generic
   Coi   *rawNextFtpEntry( );
   void   rawCloseFtpDir ( ); 
   int   rawLoadFtpDir  ( ); // load remote dir listing

   int   rawOpenFtpSubFile   (cchar *pmode);
   size_t rawReadFtpSubFile   (void *pbufin, size_t nBufSize);
   void   rawCloseFtpSubFile  ( );

   // http pages and files
   int    readWebHead      ( );
   bool   rawIsHttpDir     (int ilevel);
   //     rawLoadDir       ( )  // is generic
   Coi   *rawNextHttpEntry ( );
   void   rawCloseHttpDir  ( ); 
   bool   isHttpDirByName  (char *psz);

   int   rawOpenHttpSubFile  (cchar *pmode);
   size_t rawReadHttpSubFile  (void *pbufin, size_t nBufSize);
   void   rawCloseHttpSubFile ( );

   #endif // VFILEBASE

   int   applyWriteCloseTime( );

   bool  debug    ( );

   // core data for every lightweight Coi:
   char  *pszClName;    // ansi or utf
   char  *pszClUName;   // just utf
   ushort *pwClName;    // windows: wide char name
   char  *pszClRoot;
   char  *pszClRef;
   char  *pszClExtStr;

   uchar nClStatus;
   uint  nClHave;
   num   nClSize;
   num   nClMTime;   // modification time
   num   nClCTime;   // creation time, or <= 0
   bool  bClRead;
   bool  bClWrite;
   bool  bClDir;     // any dir, e.g. by name
   bool  bClFSDir;   // verified native filesystem dir
   bool  bClHidden;
   bool  bClLink;
   bool  bClBinary;
   bool  bClArc;
   uchar nClUCS;     // 0:none 0xFE:LE 0xEF:BE
   bool  bClSnap;    // sfk snapfile
   bool  bClSetWriteCloseTime;
   bool  bClBadName; // windows: after conversion
   bool  bClUniName; // set cs.uname when processing
   // after close(), set file time using MTime and/or CTime
   uint  nClAttr;    // file attributes
   uint  crc;

   // simplified infos for http cois
   int   setTypeFromHeaders      ( );
   int   setTypeFromContentType  (char *pctype);
   bool  bClWebText;
   bool  bClWebBinary;
   bool  bClWebPage;
   bool  bClWebJpeg;
   bool  bClWebPNG;
   bool  bClWebImage;

   // ON EXTENSIONS ABOVE, ADAPT COI::COPY, Coi::fillFrom!
   // also check FileStat::readFrom, writeTo

   int  nClRefs;    // not to be coi::copied

   char  szClOutExt[8]; // [f]pic only jpg or png

   #ifndef _WIN32
public: // not yet defined
   // additional informal stuff
   uint rawmode;  // for tracing
   uint rawtype;  // for tracing
   uint rawnlnk;  // link count
   num   nClINode;   // under linux
   __dev_t oClStDev; // under linux
   // file id is made from:
   //   16 bytes stdev (expanded as string)
   //   16 bytes inode (expanded as string)
   //   zero terminator
   char  szClFileID[40];
   #endif

public: // not really
   // heavyweight Coi's use this as well:
   CoiData *pdata;
   CoiData  &data    ( );
   bool   hasData    ( );  // has a CoiData object

   #ifdef VFILEBASE
   bool   bClInCache;
   bool   isCached   ( );
   bool   hasContent ( );  // has CoiData AND cached data
   #endif // VFILEBASE

   // adapt ctr and copy() on extensions!
};

#define COI_HAVE_SIZE   (1UL<<0)
#define COI_HAVE_TIME   (1UL<<1)
#define COI_HAVE_READ   (1UL<<2)
#define COI_HAVE_WRITE  (1UL<<3)
#define COI_HAVE_DIR    (1UL<<4)
#define COI_HAVE_HIDDEN (1UL<<5)
#define COI_HAVE_LINK   (1UL<<6)
#define COI_HAVE_BINARY (1UL<<7)
#define COI_HAVE_NODE   (1UL<<8)
#define COI_HAVE_ARC    (1UL<<9)
#define COI_HAVE_ATTR   (1UL<<10)

class CoiTable {
public:
   CoiTable             ( );
  ~CoiTable             ( );

   // use THIS for a simple coi list:
   int addEntry         (Coi &ocoi, int nAtPos=-1);
   // it adds a COPY of the supplied coi.

   int  removeEntry     (int nAtPos);
   int  numberOfEntries ( );
   Coi  *getEntry       (int iIndex, int nTraceLine);
   int  setEntry        (int iIndex, Coi *pcoi);
   int  addSorted       (Coi &ocoi, char cSortedBy, bool bUseCase);
   void resetEntries    ( );
   bool isSet           (int iIndex);
   int  hasEntry        (char *pszFilename);

private:
   int expand           (int nSoMuch);
   int nClArraySize;
   int nClArrayUsed;
   Coi  **apClArray;
};

#ifdef VFILEBASE
class ZipReader;
class HTTPClient;
class FTPClient;
#endif // VFILEBASE

// data for directory and archive processing:
// must be declared before ~coi otherwise data dtr is not called?
class CoiData {
public:
    CoiData  ( );
   ~CoiData  ( );

   // directory traversal data
   bool   bdiropen;
   char   *prelsubname;  // filename within directory

   // generic I/O support
   bool   bfileopen;     // between open() ... close() of files
   char   szmode[10];    // i/o mode: "rb","r+b","wb"
   bool   bwrite;        // (also) writing to file
   bool   bstoprdbin;    // stop read if binary detected
   num    ntotalread;    // bytes read since open()
   uint  ntold;         // warnings told about this file
   char   szlasterr[50]; // most recent error info
   #ifdef VFILEBASE
   bool   bloaddirdone;  // don't repeat dir loading
   bool   bstopread;
   #endif // VFILEBASE
   bool   banyread;      // anything yet read after open()?
   num    nopentime;     // time point of open, or 0

   // this buffer is for high-level Coi read functions:
   // -  readLine()
   // -  isBinaryFile()
   // it shall NOT be used by low-level read functions.
   struct CoiReadBuf {
      uchar  *data;     // alloc'ed on demand
      int    getsize;
      int    getindex;
      int    geteod;
      num     getpos;
   } rbuf;  // overlapping read cache buffer

   // this buffer can optionally cache the whole input
   struct CoiSrcBuf {
      uchar  *data;  // NULL if not yet cached
      num     size;
      num     index; // current read index
      num     time;  // src mtime
   } src;

   // native filesystem I/O
   char  *pdirpat;
   #ifdef _WIN32
   bool   bdir1stdone;
   intptr_t otrav;
   #else
   DIR     *ptrav;
   #endif
   FILE    *pfile;   // managed by Coi, not by CoiData

   #ifdef VFILEBASE
   // INTERNAL list of subfiles (zip, net).
   // elements shall NOT be passed directly
   // to callers, but only copies of them.
   CoiTable *pelements;
   CoiTable &elements ( );

   int nNextElemEntry;

   num  nPreCacheFileCnt;
   num  nPreCacheFileBytes;

   // ftp support
   FTPClient *pClFtp;
   // when set, it is allocated by the coi,
   // and must be released after use.
   int  getFtp(char *purl);
   // on rc0, can use pClFtp.
   int  releaseFtp( );
   // does NOT close connection, but clears pClFtp.

   // http support
   HTTPClient *pClHttp;
   // when set, it is allocated by the coi,
   // and must be released after use.
   int  getHttp(char *purl);
   // on rc0, can use pClHttp.
   int  releaseHttp( );
   // clears pClHttp, but if connection is
   // in keep-alive, does NOT close it yet.
   StringMap &headers( );
   // http headers, so far only non-redundant entries
   // like content-type, but not multiple set-cookies.
   StringMap *pClHeaders;

   // if the coi was http redirected, then
   bool  bRedirected;   // this is true
   char  *pClOrgName;   // this is the first coi name
   #endif // VFILEBASE
};

class CoiAutoDelete {
public:
      CoiAutoDelete (Coi *pcoi, bool bDecRef)
         { pClCoi = pcoi; bClDecRef = bDecRef; }
     ~CoiAutoDelete ( ) {
         if (!pClCoi)
            return;
         if (bClDecRef)
            pClCoi->decref();
         if (!pClCoi->refcnt())
            delete pClCoi; 
      }
private:
      Coi *pClCoi;      // can be NULL
      bool bClDecRef;   // on dtr, do a single decref
};

class AutoCoiDirClose {
public:
      AutoCoiDirClose (Coi *pcoi) { pClCoi = pcoi; }
     ~AutoCoiDirClose ( ) {
         if (pClCoi->isDirOpen()) {
            mtklog(("auto-close coi %p", pClCoi));
            pClCoi->closeDir();
         }
      }
   Coi *pClCoi;
};

enum eProgressInfoKeepFlags {
   eKeepProg   = (1<<0),
   eKeepAdd    = (1<<1),
   eNoCycle    = (1<<2),
   eSlowCycle  = (1<<4),
   eNoPrint    = (1<<5)
};

class ProgressInfo
{
public:
   ProgressInfo          ( );
   void  setWidth        (int nColumns);
   void  setAddInfoWidth (int nColumns); // abs. columns, high prio
   void  setAddInfo      (const char *pszFormat, ...);
   void  setAction       (cchar *pszVerb, cchar *pszSubject, cchar *pszAddInfo=0, int nKeepFlags=0);
   void  setStatus       (cchar *pszVerb, cchar *pszSubject, cchar *pszAddInfo=0, int nKeepFlags=0);
   void  print           ( );  // print status now, keep line
   void  printLine       (int nFilter=0); // print final status, including newline
   void  cycle           ( );  // print status if enough time elapsed
   void  clear           ( );  // clear status, if it was printed
   int   print           (const char *pszFormat, ...);
   void  setProgress     (num nMax, num nCur, cchar *pszUnit, bool btriple=0);
   void  setStatProg     (cchar *pverb, cchar *psubj, num nMax, num nCur, cchar *pszUnit);
   void  clearProgress   ( );

// private:
   void  fixAddInfoWidth ( );
   void  dumpTermStatus  ( );
   void  clearTermStatus ( );  // if anything to clear
   int  nMaxChars;
   int  nMaxSubChars;
   int  nAddInfoCols;
   int  nDumped;
   num   nLastDumpTime;
   bool  bAddInfoPrio;
   int  nAddInfoReserve;
   int  nTurn;
   char  szVerb   [200];
   char  szSubject[200];
   char  szAddInfo[200];
   char  szTermBuf[400];
   char  szPrintBuf[400];
   char  szPerc   [20];
};

extern ProgressInfo info;

// simple double linked list

class ListEntry
{
public:
    ListEntry   ( );
   ~ListEntry   ( );

   ListEntry *next      ( ) { return pClNext; }
   ListEntry *previous  ( ) { return pClPrevious; }

   ListEntry *pClNext;
   ListEntry *pClPrevious;

   // user payload
   void  *data;
};

class List
{
public:
   List  ( );
  ~List  ( );

   ListEntry *first  ( ) { return pClFirst; }
   ListEntry *last   ( ) { return pClLast; }

   void add          (ListEntry *p);
   void addAsFirst   (ListEntry *p);
   void addAfter     (ListEntry *after, ListEntry *toadd);
   void remove       (ListEntry *entry);
   void reset        ( );
   int  size         ( );

private:
   ListEntry *pClFirst;
   ListEntry *pClLast;
};

#ifdef SFK_PROFILING // windows only

class StaticPerformancePoint;

#define MAX_PERF_POINTS 100

class StaticPerformanceStats
{
public:
      StaticPerformanceStats  ( );

   void  addPoint       (StaticPerformancePoint *pPoint);
   int   numberOfPoints ( );
   StaticPerformancePoint *getPoint(int iIndex);

StaticPerformancePoint
   *apClPoints [MAX_PERF_POINTS+10],
   *pClCurrentPoint;
int
    iClPoints;
};

extern StaticPerformanceStats glblPerfStats;

class StaticPerformancePoint
{
public:
      StaticPerformancePoint  (const char *pszID, const char *pszFile, int iTraceLine);

      inline void  blockEntry ( );
      inline void  blockExit  (int iElapsedTicks);

const char *pszClID;
const char *pszClFile;
int   iClTraceLine;
num   iClHits;
num   iClTotalTime;
num   iClSubTimes;
};

class DynamicPerformancePoint
{
public:
       DynamicPerformancePoint (StaticPerformancePoint *pStaticPoint);
      ~DynamicPerformancePoint ( );

StaticPerformancePoint
      *pClStaticPoint,
      *pClStaticParent;
num
       nClEntryTickCount;
};

#define _p2(id,file,line)  \
   static StaticPerformancePoint oStatPoint##line(id,file,line); \
   DynamicPerformancePoint oDynaPoint##line(&oStatPoint##line);

#define _p(id) _p2(id,__FILE__,__LINE__)

extern void logProfile();

inline num getPerfCnt()
{
   LARGE_INTEGER val1;
   QueryPerformanceCounter(&val1);
   return val1.QuadPart;
}

inline num getPerfFreq()
{
   LARGE_INTEGER val1;
   QueryPerformanceFrequency(&val1);
   return val1.QuadPart;
}

#else

#define _p(id)

extern void logProfile();

#endif

int getFileSystemInfo(
   char  *pszPath,         // e.g. "D:\\", "/home/user/"
   num   &nOutTotalBytes,  // total volume size
   num   &nOutFreeBytes,   // free bytes usable for normal users
   char  *pszOutFSName,    // file system name buffer
   int   nOutFSNMaxSize,   // size of this buffer
   char  *pszOutVolID,     // volume name and serial, if any
   int   nOutVolIDMaxSize, // size of this buffer
   uint &rOutVolID
   );
   
int createOutDirTree(char *pszOutFile, KeyMap *pOptMap=0, bool bForDir=0);

#ifdef _WIN32
void timetToFileTime(num ntimet, FILETIME *pft);
num fileTimeToTimeT(num nwft);
num fileTimeToTimeT(FILETIME *pft);
#endif

inline void sfkSetBit(uchar *pField, uint iBit) 
{
   pField[iBit>>3] |= (1U << (iBit & 7));
}

inline uchar sfkGetBit(uchar *pField, uint iBit)
{
   return (pField[iBit>>3] & (1U << (iBit & 7))) ? 1 : 0;
}

extern int (*pGlblSFKStatusCallBack)(int nMsgType, char *pmsg);

char *dataAsHex(void *pAnyData, int iDataSize, char *pszBuf=0, int iMaxBuf=0, bool bLowerCase=0);
char *dataAsTrace(void *pAnyData, int iDataSize=-1, char *pszBuf=0, int iMaxBuf=0);
char *dataAsTraceW(ushort *pAnyData);

/*
    Simplest possible utf8 decoder, primarily for 16 bit code points.
    No support for surrogates or any complex sequences.

    UTFDecoder odec(szInText);
    while (odec.haveChar())
    {
        uint u = odec.nextChar();
        ...
    }
*/
class UTF8Codec
{
public:
   UTF8Codec   (char *pOptInData=0, int iOptionalInputLength=-1);

   void  init  (char *pInputData, int iOptionalInputLength=-1);

   static int toutf8 (char *pszOut, int iMaxOut, uint ch);
   static int toutf8 (char *pszOut, int iMaxOut, char *pszIsoText, bool bSafe=0);
   static bool isValidUTF8 (char *psz);

   bool  hasChar();
   uint  nextChar();
   bool  eod();

   static int  validSeqLen    (char *pszSrc, int iMaxSrc);
          int  validSeqLenInt (char *pszSrc, int iMaxSrc);

   int   readRaw();
   int   readSeq();

   int   icur, imax;
   bool  banychars;
   bool  bbadchars;
   bool  bdecodexml;
   bool  bkeeputf;   // sfk1942
   uchar *psrc;
};

class SFKMatch;

struct CommandStats
{
public:
   CommandStats   ( );
   void reset     ( );
   bool showstat  ( );

   int debug     ;
   int memcheck  ;
   int verbose   ;  // 0,1,2
   int iotrace   ;
   int tracechain;
   bool shortsyntax        ; // sfk1812 i/o bGlblShortSyntax
   bool anyused            ; // sfk1812 i/o bGlblAnyUsed
   bool delStaleFiles      ;
   bool skipOwnMetaDir     ;
   bool blockAutoComplete  ;
   int tabSize   ;
   int tabsDone  ;
   int tabFiles  ;
   int scanTabs  ;
   bool scanIndent;
   int indentFilt;

   int files     ; // visible plus hidden
   int filesChg  ; // no. of files changed
   int filesZip  ; // no. of files (un)zipped
   int noFiles   ; // fnames that failed to stat etc.
   int dirs      ; // visible plus hidden
   int filesCloned ; // no. of files with attributes copied
   int dirsCloned  ; // no. of dirs with attributes copied
   bool hidden    ; // include hidden files and dirs
   int numHiddenFiles ; // for list stats
   int numHiddenDirs  ; // for list stats
   int numHiddenFilesSkipped ;
   int numHiddenDirsSkipped  ;
   int numBadFileNames; // unreadable unicodes
   int binariesSkipped ;
   int addedFilesSkipped ; // on -sincedif
   int shadowsWritten ;
   int shadowFallbacks ;
   int filesDeleted ;
   int filesDeletedWP;
   int dirsDeleted  ;
   int dirsDelFailed;
   int dirsDeletedWP;
   int filesScanned ;
   int dirsScanned  ;
   int filesMoved   ;
   int filesDelFailed;
   int filesNewerInDst ;
   int filesStale ; // deletion candidate
   int filesRedundant;  // rename
   int filesExisting;   // rename
   int noOutDir;        // rename
   int badOutDir;       // rename
   int lines    ;
   num  maxFileTime;
   uint listForm;    // list -size etc.
   bool listTabs;    // split columns by tab char
   bool listContent; // list zip etc. info
   int  flatTime;    // show flat file times
   bool sim   ;      // just simulate command
   bool nohead;      // leave out some header, trailer info
   bool pure  ;      // extra info if -pure was specified
   bool dostat;      // copy: list just size statistics
   bool tailTail;    // running tail, not head
   int tailLines;    // head, tail
   bool tailFollow;  // head, tail
   char *tomask;     // output filename mask
   char *todir;      // output dir
   bool  tomaskfile; // -to mask is a single filename
   char *overallOutFilename; // name for info
   char tomake[200]; // option -tomake
   char curcmd[50+10]; // current command. sfk1834 no pointer
   bool rootrelname; // use filenames relative to root dir
   bool rootabsname;  // copy
   bool forceabsname; // list
   bool writeall;    // write all files, not only changed ones
   bool spat;        // enable slash patterns \t etc.
   bool wpat;        // support * and ?
   bool xpat;        // dummy within base
   bool usecase;     // case-sensitive search or not
   bool fuzz;        // fuzzy case search
   bool nocase;      // optional: forced nocase on binary search
   int blankRunFiles;  // no. of filenames w/blanks passing run
   int wrongpcRunFiles;// no. of filenames w/wrong path chars
   int badNameForm;    // set by execRunFile on bad filename formats
   bool nocheck;     // do not perform any checks
   bool noinfo;      // do not tell infos
   bool nochain;     // disable command chains
   int  useJustNames;// create a list of filenames
   int  useNotNames; // sfk198 list filenames not containing
   bool countMatchLines; // count no. of matching lines
   bool yes;
   bool logcmd;
   int  force;
   bool nostop;      // command specific
   bool keepchain;   // keep chain always running
   bool syncFiles;   // sync files instead of copy
   bool syncOlder;   // with sync, copy older over newer files
   int  flat;        // copy: flat output filenames
   char cflatpat;    // how to join flat output name
   bool nonames;     // do NOT print/pass :file records
   bool subnames;    // print .xlsx subfile names
   bool noind;       // no indentation
   char *runCmd;     // default: "" if not set.
   bool printcmd;    // run: print raw command
   int stoprc;      // run: stop on rc >= stoprc
   bool anymatches;  // find: found at least 1 matching line in 1 file
   bool showrc;      // print rc at program end
   bool deplist;     // deplist command selected
   int refsrccnt;   // reflist, deplist: no. of sources
   bool depsingle;   // process dependencies of a single file
   bool coldstnames; // reflist, deplist: execRefColSrc also collects DstNames
   bool refstripsrc; // strip source file contents from unused chars
   int listByTime;
   bool listByTimeAll;
   int listBySize;
   bool listBySizeAll;
   int listByName;
   bool listByNameAll;
   bool tellExecTime;
   int timeOutMSec;
   bool timeOutAutoSelect;
   num  selMinSize;  // consider only files >= so many bytes
   bool nowarn;      // disable all warning output
   bool noerr;       // disalbe all error output
   bool showerr;     // sft
   bool nonotes;     // disalbe all note output
   bool skipLinks;   // do not follow symbolic directory links
   bool traceFileFlags;
   bool fileMaskAndMatch;  // AND match of file mask parts
   bool dirMaskAndMatch;  // AND match of path mask parts
   bool incFNameInPath;    // include filename in path mask check
   bool verifyEarly;       // copy: verify directly after write
   bool verifyLate;        // copy: verify in a separate pass
   FILE *outfile;          // can be used by chain.print
   bool listTargets;       // force target name listing i/o src
   int idleMode;          // low prio processing, 0 (off) to 2
   int walkDirDelay;      // low prio file processing with delays
   int walkFileDelay;     // low prio file processing with delays
   int treeStopRC;        // stop tree processing on internal RC >= this
   bool stopTree(int nrc, bool *psilent=0); // tells if to stop on the supplied rc
   bool toldTreeStop;
   bool skipDirFileColl;   // optim: do not collect flist per dir.
   // cannot be set w/functions that strictly need those lists.
   bool rcFromError;       // change shell rc on skipped errors
   bool repDump;           // replace: create hexdump of hits
   bool repDumpHalve;      // replace: hexdump only source side
   bool useFirstHitOnly;   // skip to next file after first hit
   bool withdirs;          // include directories in command
   bool withrootdirs;      // if withdirs is used, include root dirs?
   bool justdirs;          // process only directories
   bool predir;
   bool usesnap;           // interpret snapfile format and list titles
   bool usesnapfiltname;   // filter filenames as well
   int addsnapraw;        // snapto raw mode 1 or 2
   const char *addsnaplf;  // "\n" or "\r\n" depending on mode and OS
   uint addsnapmeta;      // bit 0:time 1:size 2:encoding
   int stathilitelevel;   // stat command: highlight dirs <= this
   bool travelzips;        // traverse zipfile contents
   int  office;            // traverse office contents
   int  justoffice;        // select just office files
   bool infilelist;        // processing a file list, not dir and mask
   bool probefiles;        // look into file headers to detect zip etc.
   bool incbin;            // include all binary files in processing
   bool incwlbin;          // include white listed binary files
   bool reldist;           // hexfind: tell also relative distances
   #ifdef VFILEBASE
   bool shallowzips;       // list only first level of zips
   bool precachezip;
   bool extdomref;         // include external domain refs
   bool xelike;            // set xe default behaviour and help text
   bool cacheall;          // no direct processing of files
   bool cachestat;         // cache statistics at program end
   bool travelHttp;        // decided per command, esp. list
   #endif // VFILEBASE
   bool  recurl;
   bool subdirs;           // recurse into subdirs
   bool hidesubdirs;       // do not process subdir names at all
   bool utf8dec;           // utf-8  detect and decode (not yet impl.)
   bool wchardec;          // utf-16 detect and decode
   int utf16found;         // statistic for post-command info
   int utf16read;          // statistic for post-command info
   bool showdupdirs;       // linux: tell if dir link contents are skipped
   bool usecirclemap;      // linux: allow circle map, on by default
   num  sincetime;         // process only files modified since that time
   num  untiltime;         // process only files modified until that time
   bool usectime;          // use creation time instead of modification time
   bool useutc;            // all times in UTC/GMT instead of local
   char paramprefix[30];   // for user defined script input parameter names
   int wrapcol;            // if >0, auto-wrap lines in snapfile
   int wrapbincol;         // only on binary to text conversion
   num nlineswrapped;      // number of hard wraps
   bool rewrap;            // ignore linefeeds, rewrap all
   char listunit;          // stat output in 'b'ytes, 'k'bytes or default.
   bool flatdirstat;       // list no. of files per dir, not dir tree
   int flatfilecnt;        // global stats if flatdirstat is set
   int flatdircnt;         // "
   num  flatbytecnt;       // "
   bool statonlysum;       // sfk stat: quiet except summary
   int quiet;              // quiet mode
   bool ftpupdate;         // mput, mget: explicite -update
   bool ftpall;            // mput, mget: disable -update mode
   bool webdesklist;       // with webserv
   bool noclone;           // disable time stamp replication
   bool preserve;          // copy full attributes with sft
   int fast;               // command dependent optimization
   bool verify;            // command dependent optimization
   bool prog;              // with progress indicator
   bool noprog;            // no progress indicator
   // bool notext;         // no result text (never used)
   bool test;              // filter: run in test mode
   bool copyLinks;         // copy symlinks     , windows only, untested
   bool copyNoBuf;         // copy w/o buffering, windows only, untested
   bool copyDecrypt;       // copy and decrypt  , windows only, untested
   bool rerun;             // sfk rerun
   bool textfiles;         // process only textfiles
   bool binaryfiles;       // process only binaryfiles
   bool packalnum;         // deblank: reduce filenames to alnum
   bool noipexpand;        // disallow ip number expansion
   int  stopcnt;           // stop command after n events
   char szownip[60];       // manually set own ip
   bool anyFileTooLarge;   // info after command execution
   bool crashtest;         // enforce crash to test handling
   bool justvernum;        // version command
   bool separator;         // print separator between outputs
   char szseparator[100];  // with xfind
   bool nolf;              // skip lf output on some commands
   bool multicast;         // udpclient
   int  dumptrail;         // hexdump: trailing chars at line end
   int  bytesperline;      // hexdump: when using hex/decsrc
   num  recordsize;        // for some commands
   bool usetmp;            // use temporary file
   bool knx;               // internal
   char *knxtext;          // internal
   bool ntp;               // internal
   bool echoonerr;         // echo whole command on error
   int  argc;              // copy of main() argument
   char **argv;            // copy of main() argument
   int  selfilenum;        // current processed file number
   int  selfileoff;        // process only files from this offset
   int  selfilerange;      // process only so many files
   bool stopfiletree;      // stop dir tree processing silently
   bool showip;            // show automatic ip expansion result
   bool justrc;            // no terminal output on filter
   num  minsize;           // select only files >= that size
   num  maxsize;           // select only files <= that size
   bool keeptime;          // keep input filetime on output file
   uint timemask;          // bit mask of what times to list
   bool tabform;           // use tab separators
   bool autoclose;         // ftpserv: on second client
   num  diskspace;         // required free disk space for writing
   bool xchars;            // treat \xnn as characters
   bool extract;           // replace, hexfind
   FILE *extractOutFile;   // ""
   int  xmaxlen;           // xpat default maxlen
   int  xmaxlit;           // xpat max literal size
   bool nodirtime;         // copy should not clone dir times
   bool collines;          // sfk193 with xed
   bool fixedbylist;       // force fixed record -bylist file
   bool showpre;           // replace
   bool showpost;          // replace
   bool showlist;          // replace
   bool rawfilename;       // with hexdump
   bool hexfind;           // running (x)hexfind
   bool xtext;             // running xtext
   bool xfind;             // running xfind
   char placeholder;       // for null bytes
   bool rawterm;           // dump output as is
   bool usefilehead;       // use mask given below
   char szfilehead[200];   // per result file header with "%s" internal
   int  maxdump;           // used differently
   bool fullhelp;
   int  reprep;            // repeat replace option
   bool perf;              // performance statistics
   char szeol[10];         // crlf or lf
   bool toiso;             // utf8 to iso conversion
   char toisodef;          // default character '.'
   bool toutf;             // iso to utf8 conversion
   char *delim;            // list of delimiters for soft word wrapping
   bool astext;            // with xhexdump
   bool joinlines;         // with find
   int  rtrim;             // with find
   bool nostat;            // xhexfind: no no. of hits statistics
   char litattr;           // literal highlight attribute, or 0 for none
   char leattr;            // line end attribute, or 0 for none
   bool leauto;            // auto detect text file then set leattr
   bool forcele;           // force line endings with addcr/remcr
   int  fastopt;           // fast option, function specific
   // csvtotab, tabtocsv
   char cinsep;
   char coutsep;
   char cquote;
   char coutsepesc;
   bool quotetext;
   bool quoteall;
   int  contextlines;      // xfind: 1=currentline 2=previous and post line
   int  contextchars;      // max chars of all context lines together
   int  indent;
   char *renexp;           // rename expression
   char *rentodir;         // rename moveto dir
   bool exact;
   bool listfiles;
   bool uname;             // windows: utf names
   bool unameauto;
   bool unameout;
   bool showrawname;
   bool tname;             // windows: transcript names
   bool aname;             // windows: ansi from wide char read
   bool dewide;            // fixfile
   bool rewide;            // fixfile
   bool setftime;          // fixfile
   bool setndate;          // fixfile
   bool dumpfrom;          // (x)replace
   bool dumpboth;          // (x)replace
   num  maxscan;           // (x)replace
   bool nodump;            // udpdump -forward
   bool prefix;            // udpcast -prefix
   int  maxwebwait;        // network
   int  maxftpwait;        // network
   bool upath;             // run
   char outpathchar;       // sfk198
   char *puser;
   char *ppass;
   char *pwebuser;         // sfk198
   char *pwebpass;         // sfk198
   bool errtotext;
   bool trimscript;
   char mlquotes;          // multi line quotes format
   int  showhdr;           // print web headers
   bool showreq;           // print web requests
   char *headers;          // sfk1972 user web headers prefixed by \n
   char *webreq;           // sfk1972 full predefined web request
   int  webreqlen;         // sfk1972 with -reqfromvar
   num  maxwebsize;        // web download limit
   bool execweb;
   bool openbyapp;
   int  maxlines;          // max lines to read
   int  taillines;         // lines from eof
   bool usevars;
   bool quotevars;
   bool sellines;          // select lines from input
   int  linesfrom;         // if sellines
   int  linesto;           // if sellines
   int  strict;            // run, -to, perline mask
   bool relaxedvar;        // for filter
   bool fullheader;        // (x)hexfind, (x)rep etc.
   bool winver;
   bool nosft;             // force ftp protocol
   bool allowsft;          // allow sft protocol
   bool showprotocol;
   bool brackets;
   bool cweb;              // +web expects chain input
   bool movefiles;
   bool withempty;
   SFKMatch *apexp;        // xrename
   int  iexp;              // xrename
   num  totalinbytes;
   num  totaloutbytes;
   num  totalbytes;        // sfk1934
   int  icomp;
   bool fastcomp;
   int  numBadFiles;
   int  numFilesOK;
   bool usecolor;          // sfk189
   bool usehelpcolor;      // sfk189
   #ifdef SFKPACK
   void *zfout;
   bool bzip2;
   bool force64;
   bool catzip;
   Coi  *pOutCoi;
   bool toziplist;
   bool hidezipcomment;
   bool addmeta;
   char *tozipname;
   uint nzipredundant;
   uint mofftime;          // apply for 0:dos 1:unix 2:ntfs
   bool bjustcrc;
   uint njustcrc;
   #endif // SFKPACK
   int  tracecase;
   bool nocasemin;         // sfk190 just latin a-z
   bool binallchars;       // sfk190 for binary extracts
   bool outcconv;
   bool forcecconv;
   int  ifailedchars;
   int  iinvalidutfmarks;
   int  inonutfhicodes;
   bool nozipmeta;
   bool keepbadout;
   int  iutfnames;
   int  iexecfiles;
   bool bzipto;
   bool bnoextutf;
   bool utfout;            // sfk1942
   int  nmore;
   int  imore;
   int  morepage;
   int  makemd5;
   bool crcmd5;            // use crc instead of md5
   bool keepdata;          // keep chaindata with setvar
   char *notifyto;         // sft(serv)
   int  justevery;
   int  everycnt;
   bool sanecheck;         // crccheck -sane
   num  sanetime;          // of crc list file
   int  usehta;            // webserv internal
   bool usingflist;        // sfk196 with -flist
   int  absdirs;           // sfk1963 with sync
   bool checkdirs;
   char rootdir[SFK_MAX_PATH+10];   // sfk197 -root
   char setxmask[60];      // draft: -setexec
   char curhost[255+20];   // sfk197
   int  curport;           // sfk197 -1 if unset
   int  chan;              // sfk1972 tcp channel, from 1
   int  chanserv;          // sfk1972 server channel
   bool noqwild;           // sfk1972
   bool nopass;            // sfk1972 internal
   bool procpic;           // support $outext
   int  cliptries;
   int  xxencode;
   int  webnoclose;         // sfk198 for testing
   char webbasedir[512+10]; // sfk198 webserv
   char webcurdir[512+10];  // sfk198 webserv
   char *pwebstartdir;
   #ifdef SFKPIC
   bool deeppic;           // probe file start
   bool dumppix;
   int  minwidth;
   int  minheight;
   int  dstpicwidth;       // cs.pic
   int  dstpicheight;
   int  dstpicquality;
   uint picloadflags;
   uint backcol;
   bool notrans;
   bool detrans;
   int  dstchan;
   int  nopicfiles;
   int  pnginfiles;
   int  jpginfiles;
   int  pngoutfiles;
   int  jpgoutfiles;
   int  rgboutfiles;
   int  srcpicwidth;
   int  srcpicchan;
   char *pszgallery;
   FILE *pfgallery;
   #endif // SFKPIC
};

// extern struct CommandStats gs;
// extern struct CommandStats cs;

enum eWalkTreeFuncs {
   eFunc_MD5Write = 1,
   eFunc_JamFile  = 2,  // fixed value
   eFunc_CallBack = 3,  // fixed value
   eFunc_Detab       ,
   eFunc_Entab       ,
   eFunc_JamIndex    ,
   eFunc_SnapAdd     ,
   eFunc_FileStat    ,
   eFunc_FileTime    ,
   eFunc_Touch       ,  // 10
   eFunc_Find        ,
   eFunc_Mirror      ,  // deprecated
   eFunc_Run         ,
   eFunc_FormConv    ,
   eFunc_Inst        ,
   eFunc_RefColSrc   ,  // collect reflist sources
   eFunc_RefColDst   ,  // collect reflist targets
   eFunc_Deblank     ,
   eFunc_FTPList     ,
   eFunc_FTPNList    ,  // 20
   eFunc_FTPLocList  ,
   eFunc_Hexdump     ,
   eFunc_Copy        ,
   eFunc_Cleanup     ,
   eFunc_AliasList   ,
   eFunc_ReplaceFix  ,  // 26
   eFunc_ReplaceVar  ,  // 27
   eFunc_MetaUpd     ,
   eFunc_MetaCheck   ,
   eFunc_Scantab     ,
   eFunc_Filter      ,
   eFunc_Load        ,
   eFunc_Delete      ,
   eFunc_DupScan     ,
   eFunc_Version     ,
   eFunc_Media       ,
   eFunc_XHexDemo    ,
   eFunc_Rename      ,
   eFunc_XRename     ,
   eFunc_GetPic      ,
   eFunc_XFind       ,
   eFunc_Move        ,
   eFunc_SumFiles    ,
   eFunc_UUEncode    ,
   eFunc_Pic
   #ifdef SFKPACK
   , eFunc_ZipTo
   #endif // SFKPACK
};

// temporary file class, REMOVING THE FILE IN DESTRUCTOR.
class SFTmpFile
{
public:
   SFTmpFile   (const char *pszExt, bool bNoAutoDelete, uint nTmpFileNum = 0);
  ~SFTmpFile   ( );
   char *name  ( );
   static void setTmpDir(char *pszDir);
   static bool tmpDirWasSet( );
private:
   bool  bClAutoDel;
   uint nClNum;
   char szClExt[100];
   char *pszClName;
   static int ncnt;
   static char *pszTmpDir;
};

class FileStat {
public:
   FileStat       ( );
   int  readFrom (char *pszSrcFile, bool bWithFSInfo=0, bool bSilent=0);
   int  writeTo  (char *pszDstFile, int nTraceLine, bool bWriteJustTime=0);
   int  differs  (FileStat &oref, bool bSameIfOlderSrc, bool *pSrcIsOlder=0);
   int  copyFrom (FileStat &src);
   int  setFilename(char *psz);
   char *filename( );
   int  writeStat(int iTraceLine); // using stored filename
   int  dump     ( );
   int  dumpTimeDiff (FileStat &rdst);
   num   getSize  ( )   { return src.nSize; }
   uchar *marshal (int &nRetSize);
   int  setFrom  (uchar *pBuf, int nBufSize);
   char  *attrStr ( );
   const char *diffReason  (int nReason);

   num   getUnixTime ( ) { return src.nMTime; }
   num   getWinTime  ( );

public:
   int   dumpSub  (int nRow, uint nmask, char *pszOut, int iMaxOut);
   void  reset    ( );

   struct FileStatSrcInfo
   {
      int  bIsDir;
      int  bIsReadable;
      int  bIsWriteable;
 
      // time of MODIFICATION or LAST WRITE is ALWAYS available.
      num   nMTime;
 
      // time of CREATION and LAST ACCESS is only available on
      // SOME file systems, e.g. NTFS. on FAT32, ATime is 0,
      // and CTime == MTime.
      num   nCTime;
      num   nATime;
 
      #ifdef _WIN32
      FILETIME ftMTime;
      FILETIME ftCTime;
      FILETIME ftATime;
      bool  nHaveWFT;   // 1 = have at least windows mod file time
      #endif            // 2 = also have windows C and A time
 
      num   nSize;
      int  bIsUTCTime;
      uint nAttribs;
   }
   src;

   char  szClFileName[SFK_MAX_PATH+10];
   char  szClSrcPath[SFK_MAX_PATH+10];
   char  szClSrcFSName[200];
   char  szClSrcVolID[200];
   char  szClTextBuf1[200];
   char  szClTextBuf2[200];
   char  szClAttrStr[50];
   char  szClDiffReason[100];
};

#define MAX_MOV_CMD 100
#define SFKMOV_KEEP   1
#define SFKMOV_CUT    2

class Media
{
public:
      Media ( );

   void  reset             ( );
   void  closeOutput       ( );
   void  clearCommands     ( );
   void  shutdown          ( );
   int   parseM3UFile      (char *pszFilename);
   int   processMediaFile  (char *pszSrc, char *pszOutFile);
   int   findSeconds       (num nBytePos); // with M3U only
   int   renderTempName    (char *pszFromname);
   int   analyze           (uchar *pbuf, int isize);
   void  setFixParms       (char *psz);

static Media *pClCurrent;
static Media &current ( );

int   aCmd[MAX_MOV_CMD];
num   aBeg[MAX_MOV_CMD];
num   aEnd[MAX_MOV_CMD];
int   aBegSec[MAX_MOV_CMD];
int   aEndSec[MAX_MOV_CMD];
int   iFirstCmd,iCmd;
int   iClInvalidFiles;
int   iClDoneFiles;
int   iClDoneTS;
bool  bClHaveM3UCommands;
bool  bClHaveKeep;
bool  bClKeepAll;
bool  bClJoinOutput;
bool  bClFixOutput;
bool  bClScan;
bool  bClKeepTmp;
bool  bClShowTmp;
num   nClGlobalBytes;

char  szClTmpOutFile[SFK_MAX_PATH+10];
char  szClRecentOutFile[SFK_MAX_PATH+10];
char  szClFinalFile[SFK_MAX_PATH+10];
char  szClFixParms[200];

char  szClMoveSrcOutDir[200];
char  szClMoveSrcOutFile[SFK_MAX_PATH+10];

FILE  *fClOut;
FileStat clOutStat;

char  *pszClM3UText;
char  *pszClM3UFileEntry;  // pointer into M3UText
};

class FileCloser {
public:
    FileCloser  (Coi *pcoi); // can be NULL
   ~FileCloser  ( );
private:
    Coi *pClCoi;
};

class CommandChaining
{
public:
   CommandChaining ( );

   bool  colfiles;   // collect filenames
   bool  usefiles;   // use collected filenames
   bool  coldata;    // collect data
   bool  usedata;    // use collected data
   bool  colbinary;  // with coldata: next command accepts binary

   CoiTable *infiles;   // while using filenames
   CoiTable *outfiles;  // while collecting filenames

   StringPipe *indata;  // text and attributes
   StringPipe *outdata; // text and attributes
   StringPipe *storedata;
   bool        storetextdone;
   StringPipe *perlinein;
   StringPipe *perlineout;

   KeyMap *justNamesFilter;

   bool  text2files;
   bool  files2text;

   int  init();
   void  reset();    // per loop
   void  shutdown();
   bool  colany() { return colfiles || coldata; }
   bool  useany() { return usefiles || usedata; }
   int  moveOutToIn(char *pszCmd);
   int  convInDataToInFiles ( );

   int  addLine(char *pszText, char *pszAttr, bool bSplitByLF=0);
   int  addToCurLine(char *pszWords, char *pszAttr, bool bNewLine=0);
   int  addStreamAsLines(int iCmd, char *pData, int iData);
   int  addBlockAsLines(char *pData, int iData);

   int  addFile(Coi &ocoi); // is COPIED
   int  hasFile(char *psz);
   int  numberOfInFiles() { return infiles->numberOfEntries(); }
   Coi  *getFile(int nIndex); // returns null on wrong index

   int  print(char cattrib, int nflags, cchar *pszFormat, ...);
   int  print(cchar *pszFormat, ...); // multi-line support

   int  printFile(cchar *pszOutFile, bool bWriteFile, cchar *pszFormat, ...);

   void  dumpContents();   // to terminal

   int   addBinary(uchar *pData, int iSize);
   uchar *loadBinary(num &rSize); // owned by caller

   int openOverallOutputFile(cchar *pszMode);
   void closeOverallOutputFile(int iDoneFiles=0);

   num   nClOutBinarySize;  // for binary write
   num   nClInBinarySize;   // for binary read
   uint  nClOutCheckSum;
   uint  nClInCheckSum;

private:
   // prebuf is huge to allow long chain.print examples
   char  szClPreBuf[MAX_LINE_LEN*2+10];
   char  szClPreAttr[MAX_LINE_LEN*2+10];
   // buf is a normal line buffer
   char  szClBuf[MAX_LINE_LEN+10];
   char  szClAttr[MAX_LINE_LEN+10];
   char  szClBinBuf[32768+100];
   int   iClBinBufUsed;
   bool  btold1;
};

extern CommandChaining chain;

enum eConvTargetFormats
{
   eConvFormat_LF     = 1,
   eConvFormat_CRLF   = 2,
   eConvFormat_ShowLE = 4
};

int joinPath(char *pszDst, int nMaxDst, char *pszSrc1, char *pszSrc2, int *pFlexible=0);
void printColorText(char *pszText, char *pszAttrib, bool bWithLF=1);
char *timeAsString(num nTime, int iMode=0, bool bUTC=0);
void dumpRepOut(uchar *pSrcCtxData, int iSrcCtxLen,
   int iHitOff, int iHitLen,
   uchar *pDstData, int iDstLen,
   num nListOffset
 );
void dumpFromToSeparator();
int atomovrange(char *psz, num *pstart, num *pend);
int atomovrange(char *psz, int *pstart, int *pend, bool bUseBytes);
bool matchesDirMask(char *pszFullPath, bool bTakeFullPath, bool bApplyWhiteMasks, int iFromInfo);
int execSingleFile(Coi  *pcoi, int lLevel, int &lGlobFiles, int nDirFileCnt, int &lDirs, num &lBytes, num &ntime1, num &ntime2);
int execSingleDir(Coi  *pcoi, int lLevel, int &lGlobFiles, FileList &oDirFiles, int &lDirs, num &lBytes, num &ntime1, num &ntime2);
bool matchesCurrentRoot(char *pszDir);
int matchesFileMask (char *pszFile, char *pszInfoAbsName, int iFromInfo);
extern bool bGlblNoRootDirFiles;
int saveFile(char *pszName, uchar *pData, int iSize, const char *pszMode="wb");
int execFileCopySub(char *pszSrc, char *pszDst, char *pszShSrc=0, char *pszShDst=0);
int execFileMoveSub(char *pszSrc, char *pszDst);
int joinShadowPath(char *pszDst, int nMaxDst, char *pszSrc1, char *pszSrc2);
int createSubDirTree(char *pszDstRoot, char *pszDirTree, char *pszRefRoot=0);
int mygetpos64(FILE *f, num &rpos, char *pszFile);
int mysetpos64(FILE *f, num pos, char *pszFile);
extern int bGlblCollectHelp;

#ifdef _WIN32
char *winSysError();
int makeWinFileTime(num nsrctime, FILETIME &rdsttime, num nSrcNanoSec=0, bool bUTC=0);
#endif

extern KeyMap glblSFKVar;
bool   sfkhavevars();
int    sfksetvar(char *pname, uchar *pdata, int idata, int nadd=0);
uchar *sfkgetvar(char *pname, int *plen);
uchar *sfkgetvar(int i, char **ppname, int *plen);
void   sfkfreevars();
bool   isHttpURL(char *psz);

#ifndef USE_SFK_BASE

 #if defined(WINFULL) && defined(_MSC_VER) && !defined(SFK_NO_MEMTRACE)
  #define SFK_MEMTRACE
 #endif

class FileMetaDB
{
public:
   FileMetaDB  ( );

   bool  canRead     ( ) { return nClMode == 1; }
   bool  canUpdate   ( ) { return nClMode == 2; }

   int  openUpdate  (char *pszFilename);
   int  openRead    (char *pszBaseName, bool bVerbose); // zz-sign w/o .dat
   int  updateFile  (char *pszName, uchar *pmd5cont = 0, bool bJustKeep=false);
   int  removeFile  (char *pszName, bool bPrefixLF = 0);
   int  updateDir   (char *pszName);
   int  save        (int &rnSignsWritten);
   void  reset       ( );
   int  checkFile   (char *pszName);
   int  numberOfFiles  ( ) { return aUnixTime.numberOfEntries(); }

   int  getFileFlags   (int nIndex) { return aFlags.getEntry(nIndex, __LINE__); }

   int  verifyFile  (char *pszFilename, char *pszShFile=0, bool bSilentAttribs=0);
   int  verifyFile  (int nIndex, bool bCleanup);
   // 0:ok 1:notfound 9:file_differs_inconsistently

   int  numberOfVerifies  ( ) { return nClVerified; }
   int  numberOfVerMissing( ) { return nClVerMissing; }
   int  numberOfVerFailed ( ) { return nClVerFailed; }
   bool  anyEvents         ( ) { return nClVerified || nClVerMissing || nClVerFailed; }
   char *filename          ( ) { return pszClDBFile; }
   int  setMetaDir        (char *psz);
   char  *metaDir          ( ) { return pszClMetaDir; }
   bool  isSignatureFile   (char *pszFile);

private:
   int  indexOf     (char *pszFile);
   int  writeRecord (FILE *fout, int nIndex, SFKMD5 *pmd5, bool bIsLastRec);
   int  writeEpilogue     (FILE *fout, SFKMD5 *pmd5);
   int  loadDB      (char *pszBasePath, bool bVerbose);
   int  loadRecord  (FILE *fin, SFKMD5 *pmd5, bool bSim); // uses szLineBuf
   int  loadHeader  (FILE *fin, SFKMD5 *pmd5);
   int  loadCheckEpilogue (FILE *fin, SFKMD5 *pmd5);

   static char *pszClFileDBHead;

   char     *pszClDBPath;
   char     *pszClDBFile;
   char     *pszClLineBuf;
   char     *pszClMetaDir;
   int     nClMode;
   int     nClVerified;
   int     nClVerMissing;
   int     nClVerFailed;
   NumTable  aUnixTime;
   NumTable  aWinTime;
   NumTable  aContSumLo;
   NumTable  aContSumHi;
   LongTable aFlags;
   StringTable aPath;

   uchar     abClRecBuf[1024];
};

extern FileMetaDB filedb;

class FileVerifier
{
public:
   FileVerifier   ( );
   int  remember (char *pszDstName, num nsumhi, num nsumlo);
   int  verify   ( );
   void  reset    ( );
   int  matchedFiles( ) { return nClMatched; }
   int  failedFiles ( ) { return nClFailed; }
   int  totalFiles  ( ) { return aClDst.numberOfEntries(); }
private:
   NumTable    aClSumHi;
   NumTable    aClSumLo;
   StringTable aClDst;
   int  nClMatched;
   int  nClFailed;
};

extern FileVerifier glblVerifier;

class CopyCache
{
public:
   CopyCache      ( );
   void setBuf    (uchar *pBuf, num nBufSize);
   int process   (char *pszSrcFile, char *pszDstFile, char *pszShDst, uint nflags);
   int flush     ( );
   void setEmpty  ( );
private:
   int putBlock  (uchar *pData, int nDataSize);
   uchar *pClBuf;
   num   nClBufSize;
   num   nClUsed;
   char  szTmpBuf[MAX_LINE_LEN+10];
};

extern CopyCache glblCopyCache;

class SFKMapArgs
{
public:
      SFKMapArgs  (char *pszCmd, int argc, char *argv[], int iDir);
     ~SFKMapArgs  ( );
   char  *eval    (char *pszExp);

   bool  bdead;

   StringTable  clDynaStrings;
   char       **clargx;
   bool         bDoneAlloc;

   char         szClEvalOut[300];   // for small results
   char        *pszClEvalOut;       // for large results
};

#ifdef SFKINT
 #define WITH_FTP_LIMITS
 // #define WITH_VAR_CALC
#endif

#define MAX_FTP_VDIR 10

class FTPServer
{
public:
      FTPServer   ( );
     ~FTPServer   ( );

   int run           (uint nPort, bool bRW, bool bRun, bool bDeep, uint nPort2, uint nPasvPort);
   char *absPath     (char *pszFilePath=0);
   char *sysPath     (char *pszFilePath=0, int *piVDir=0);
   int   mapPath     (char *pszRelPath, bool bAllowRoot=0, bool bCheckDiskSpace=0);
   int   mapPathInt  (char *pszRelPath, bool bAllowRoot, bool bCheckDiskSpace);
   int   reply       (cchar *pszMask, ...);
   int   replyFromRC (int iSubRC);
   int   readLine    ( );
   void  setStart    ( );
   int   isTimeout   (int iTimeout);
   int   addUseDir   (char *psz);
   int   setFixDir   (char *psz);

private:
   int   addTrailSlash     (char *pszBuf, int iMaxBuf);
   void  stripTrailSlash   (char *pszPath, char cSlash);
   char *notslash          (char *pszPath);
   int   setLocalWalkDir   (char *pszPath);
   int   checkPath         (char *pszPath, bool bDeep);
   int   copyNormalized    (char *pdst, int imaxdst, char *psrc);

public:
char
   szClAuthUser      [50],
   szClAuthPW        [50],
   szClRunPW         [50],
   szClEnvInfoUser   [50],
   szClEnvInfoPW     [50],
   szClEnvInfoRunPW  [50];

private:
char
   szClWorkDir    [800],
   szClOldWorkDir [800],
   szClAbsPathBuf [800],
   szClSysPathBuf [800],
   szClTmpPathBuf [800],
   szClTmpPathBuf2[800],
   szClCmpPathBuf [800],
   szClFixSysDir  [800],
   szClRenameFrom [800],
   szClReplyBuf   [1024];

int  iClVDir;
char aClVDirSrc[MAX_FTP_VDIR+2][100];
char aClVDirDst[MAX_FTP_VDIR+2][200];

#ifdef WITH_FTP_LIMITS
char aClVDirLimMode[MAX_FTP_VDIR+2];    // null, 'd'eep, 'f'lat
int  aClVDirLimFreeMB[MAX_FTP_VDIR+2];  // with 'd'eep and 'flat'
int  aClVDirLimUsedMB[MAX_FTP_VDIR+2];  // 'f'lat only
int  aClVDirLimUsedFil[MAX_FTP_VDIR+2]; // 'f'lat only
#endif

struct sockaddr_in
   clServerAdr,
   clPasServAdr,
   clClientAdr,
   clDataAdr;

SOCKET
   hClServer,
   hClClient,
   hClPasServ,
   hClData;
 
num
   nClStart,
   nClDiskFree;

bool
   bClSendFailed,
   bClTimeout,
   bClDeep;
};

#endif

// SFK home dir creation and filename building
class SFKHome
{
public:
      SFKHome  ( );

bool
      noHomeDir   ( );
char
      *makePath   (char *pszRelPath, bool bReadOnly=0),
       // also creates required folders.
       // returns NULL on any error.
      *getPath    (char *pszRelPath);
       // for readonly access.
       // returns NULL on any error.

bool  bClTold;
char  szClDir     [SFK_MAX_PATH+10];
char  szClPathBuf [SFK_MAX_PATH+10];
};

// find: BinTexter remembers so many chars from previous line
//       to detect AND patterns spawning across soft-wraps.
#define BINTEXT_RECSIZE 3000
// 600 * 2 = 1200, 1200 * 2 = 2400

class BinTexter
{
public:
   BinTexter         (Coi *pcoi);
  ~BinTexter         ( );

   enum eDoWhat {
      eBT_Print   = 1,  // floating output, LFs blanked
      eBT_Grep    = 2,  // LFs lead to hard line break
      eBT_JamFile = 3   // floating output, LFs blanked
   };

   // uses szLineBuf, szLineBuf2.
   int  process     (int nDoWhat);
   int  processLine (char *pszBuf, int nDoWhat, int nLine, bool bHardWrap);

private:
   Coi  *pClCoi;
   char  szClPreBuf[80];   // just a short per line prefix
   char  szClOutBuf[BINTEXT_RECSIZE+100]; // fix: 1703: buffer too small.
   char  szClAttBuf[BINTEXT_RECSIZE+100]; // fix: 1703: buffer too small
   char  szClLastLine[BINTEXT_RECSIZE+100];
   bool  bClDumpedFileName;
};

// --- sfk190 nocase with variable table ---

class SFKChars
{
public:
      SFKChars ( );

   int    init       ( );
   int    setocp     (ushort i);
   int    setacp     (ushort i);
   ushort getocp     ( );  // calls init().
   ushort getacp     ( );  // calls init().

   int    wlen       (ushort *puni);

   // fast calls using maps
   ushort ansitouni  (uchar  c);
   ushort oemtouni   (uchar  c);
   uchar  unitoansi  (ushort n); // returns 0 if n/a
   uchar  unitooem   (ushort n); // returns 0 if n/a

   uchar  oemtoansi  (uchar  c);
   uchar  ansitooem  (uchar  c);

   int    stroemtoansi  (char *psz, int *pChg=0, bool bNoDefault=0);
   int    stransitooem  (char *psz, int *pChg=0, bool bNoDefault=0);

   int    strunitoansi  (ushort *puni, int iunilen,
                         char *pansi, int imaxansi);

   // internal
   ushort ibytetouni (uchar c, ushort icp);

bool     bclinited;
ushort   iclocp, iclacp;
bool     bsysocp, bsysacp, banycp;

ushort   amap1[256];    // oem to uni
uchar    amap2[65536];  // uni to oem

ushort   amap3[256];    // ans to uni
uchar    amap4[65536];  // uni to ans

uchar    amap5[256];    // oem to ans
uchar    amap6[256];    // ans to oem

uchar    amap5err[256];
uchar    amap6err[256];

char     sztmp[50];
ushort   awtmp[50];
};

class SFKNoCase
{
public:
   SFKNoCase   (bool bfuzz);

   void  tellPage    ( );

   uchar itolower    (uchar c);
   uchar itoupper    (uchar c);
   uchar map1to1     (uchar c, uchar btolower, ushort *puni);
   bool  iisalpha    (uchar c);
   bool  iisalnum    (uchar c);
   bool  iisprint    (uchar c);

   // compat with xfind etc. calls
   uchar mapChar (uchar c, uchar bCase) { return bCase ? c : itolower(c); }
   uchar lowerUChar(uchar c) { return itolower(c); }
   uchar upperUChar(uchar c) { return itoupper(c); }
   void  isetStringToLower(char *psz);

bool   bclfuzz;
bool   bcltoldcp;

uchar  atolower [256];
uchar  atoupper [256];
uchar  aisalpha [256];
};

extern SFKNoCase sfknocasesharp;
extern SFKNoCase sfknocasefuzz;

#define sfkmaxname 1000

class sfkname
{
public:
   sfkname  (const char *psz, bool bpure=0);
   sfkname  (ushort *pwsz);

   char   *vname  ( );
   ushort *wname  ( );

char   szname  [sfkmaxname+20];
ushort awname  [sfkmaxname+20];
ushort nstate;
bool   bbadconv;
};

#ifdef SFKPIC

#include "sfkpicio.hpp"

class SFKPic
{
public:
   SFKPic   ( );
  ~SFKPic   ( );

   int   load     (char *pszFile);
   int   load     (uchar *pPacked, int nPacked);
   int   save     (char *pszFile);
   int   allocpix (uint w, uint h);
   void  freepix  ( );

   uint  width    ( )   { return octl.width;  }
   uint  height   ( )   { return octl.height; }

   uint  pix      (uchar a,uchar r,uchar g,uchar b);
   uchar red      (uint c) { return (uchar)(c >> 16); }
   uchar grn      (uint c) { return (uchar)(c >>  8); }
   uchar blu      (uint c) { return (uchar)(c >>  0); }
   uchar alp      (uint c) { return (uchar)(c >> 24); }

   void  setpix   (uint x, uint y, uint c);
   uint  getpix   (uint x, uint y);
   void  drawrect (int x1, int y1, int x2, int y2, uint c, int bfill);
   void  copyFrom (SFKPic *pSrc, uint x1dst, uint y1dst, uint wdst, uint hdst, uint x1src, uint y1src, uint wsrc, uint hsrc);

   int   getErrNum   ( );
   char *getErrStr   ( );

   int   getObjectSize  ( );  // for internal checks

struct SFKPicData
   octl;
};
#endif // SFKPIC

extern void sfkmem_checklist(const char *pszCheckPoint);
extern int  prepareTCP();
extern void shutdownTCP();
extern int  sfktolower(int c);
extern int  sfktoupper(int c);
extern void sfkSetStringToLower(char *psz);
extern uchar sfkMapChar(char ch, uchar bCase);
extern uchar sfkLowerUChar(uchar c);
extern uchar sfkUpperUChar(uchar c);
extern void  sfkSetHeadMatch(uchar ucFirst, uchar aHeadMatch[]);
extern char *getMacForIP(uint uiIP);
extern bool  useOfficeBaseNames();
extern void  setUsingFileList(int bYesNo);
extern bool  ispathchr(char c);
extern int   myfseek(FILE *f, num nOffset, int nOrigin);
extern int   csGetHostPort(char *psz);
extern SFKChars sfkchars;

#endif // _SFKBASE_HPP_

