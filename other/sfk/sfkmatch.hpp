/*
   SFKMatch - the Swiss File Knife Simple Expression Parser V 1.0
   ==============================================================
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#ifndef _STDINT_H
typedef unsigned char  uint8_t;
typedef unsigned short uint16_t;
typedef unsigned       uint32_t;
typedef long long      int64_t;
#endif

#define MAX_LINE_LEN               4096
#define SFKMATCH_DEFAULT_MAXLEN    MAX_LINE_LEN
#define SFKMATCH_MAX_LITERAL_SIZE  MAX_LINE_LEN

extern int SFKMatchDefaultMaxLen;
extern int SFKMatchByteWildCards;

#define SFK_LOR // sfk181

// optional user implemented callback to expand
//    [file.name]    as current input filename with path
//    [file.relname] as current input filename without path
//    [file.path]    as path of current input filename
//    [file.base]    as relname without .extension
//    [file.ext]     as input filename .extension
// Function 1: check command syntax
// Function 2: create output
// RC  0 : OK all done
// RC 10 : invalid parameters
// RC 11 : unknown command
// RC 12 : missing or invalid data, cannot execute command
typedef int (*SFKMatchOutFNCallback_t)(int iFunction, char *pMask, int *pIOMaskLen, uint8_t **ppOut, int *pOutLen);

// init flags:
#define SFKMATCH_WITHATTRIB  1  // support text color attributes

// renderOutput flags:
#define SFKMATCH_SETNOVAR    1  // interpret [setvar] but do not really set variables

enum ESFKMatchOptions
{
   SFKMatchUseCase  = 1,  // bool: case-sensitive search (not default)
   SFKMatchXChars   = 2,  // bool: treat \xnn as characters (not default)
   SFKMatchExtract  = 3,  // bool: skip unmatched data in output
   SFKMatchLitAttr  = 4,  // char: literal highlight attribute, or 0 for none
   SFKMatchTrace    = 5,  // int : print sfkmatch traces to terminal
   SFKMatchXText    = 6   // bool: running xtext (not relevant in sdk)
};

/*
   How to use:
   ===========

   -  once at program start:
         SFKMatch::globalInit();

   -  then set SFKMatch options like
         SFKMatch::bClUseCase = 1;
      to set case sensitive search

   -  then set optional "file." evaluation callback like
         SFKMatch::setOutFNCallback(cbSFKMatchOutFN);

   -  then allocate SFKMatch objects by
         pobj = new SFKMatch();
         pobj->init(pszFromText,pszToText,0);

   -  after ALL SFKMatch objects are initialized, call
         SFKMatch::provideBuffer()
      to allocated internal shared buffers using the size
      of the largest SFKMatch object.

   -  then call pobj->matches(pszInputText, iInputLength,
                  bIsFirstBlockOfInputData, bIsLastBlockOfInput)
      if will return ZERO (0) if there is a MATCH.
      it will return NON ZERO if there is NO MATCH or an ERROR.
      ERRORS are signaled by a return value of 9 or higher.

   -  if there is a match, call pobj->renderOutput(iOutLen, iSubRC)
      which will return the reformatted ToText output, it's length
      and an extra return code in case of errors.

   -  once at program end:
         SFKMatch::shutdown();

   SFKMatch uses shared buffers which are NOT thread safe.
   If you are using multiple threads in your program
   then use SFKMatch only in one of these threads.

   Short usage example:

      SFKMatch omatch;
      omatch.init("*[white]foo[white]*", "[part5]");
      omatch.provideBuffer();
 
      char *pszIn = "the foo bar";
 
      int iInLen = strlen(pszIn);
      int irc = omatch.matches((uint8_t*)pszIn, iInLen, 0, 1, 0);
 
      int iOutLen = 0;
      int iOutRC  = 0;
      char *pszOut = (char*)omatch.renderOutput(iOutLen, iOutRC);
 
      printf("match RC %d with output: \"%s\"\n", irc, pszOut ? pszOut : "?");
 
      SFKMatch::shutdown(); // at program end

   To use SFKMatch objects with color attributes:

      -  init SFKMatch objects with SFKMATCH_WITHATTRIB

      -  on every call to matches() supply pOptAttr pointing to a text
         with same size as pSrcData, containing self defined color
         codes like 'e','i','r','g','b' etc.

      -  renderOutput will then also render an attribute buffer
         that can be accessed by outAttr()

   Pattern Expansion Priorities:
      1. Start, End, LStart, LEnd, EOL
      2. Literals
      3. whitelist classes: byte of, bytes of ...
      4. blacklist classes: byte, chars, bytes, bytes not ...
*/

class SFKMatch
{
public:
   SFKMatch   ( );
  ~SFKMatch   ( );
 
static
   void  globalInit     ( ), // call once at program start
         setGlobalOption(enum ESFKMatchOptions e, int iValue);

   int   init           (char *pszFromMask, char *pszToMask, int nFlags=0);
                             // call once per object. Flags can be SFKMATCH_WITHATTRIB

   int   isValid        ( ); // returns 1 if matches() can be called

static
   int   provideBuffer  ( ), // MUST call this before first use of matches()
         shutdown       ( ); // MUST be called at program end
   int   matches        (uint8_t *pSrcData, int &rIOLength,
                         int bStart, int *pIOLineStart, int bLastRecord,
                         char *pOptAttr=0);
                             // returns ZERO (0) on a MATCH.
                             // returns NON ZERO 1...8 on a MISMATCH.
                             // returns 9 or higher on ERRORS.
  uint8_t *renderOutput (int &rOutLength, int &rRC, int nFlags=0);
                             // sets rOutLength and rRC.
                             // Flags can be SFKMATCH_SETNOVAR
  char    *outAttr      ( ); // only if color attributes are used

   int   verify         (int iPattern);
   // print console warnings on template problems

   int   objectMemory   ( );
   int   staticMemory   ( );
   int   objectFromRange( ); // from part may find up to so many bytes
   int   objectToRange  ( ); // to part may render up to so many bytes

char
   *recentPartInfo      ( ),
   *recentPrioInfo      ( );

   // optionally set user implemented function to evaluate file... parts
static int
    setOutFNCallback    (SFKMatchOutFNCallback_t pFN);

#ifndef SFKMATCH_IMPORTED
static char
   *dataAsTrace         (void *pAnyData, int iDataSize, char *pszBuf=0, int iMaxBuf=0);
#endif // SFKMATCH_IMPORTED

   char *fromText       ( );

   // when searching [lstart]foo then [lstart] is added
   // as a search for [start] or for character '\n'.
   // if [start] is found this returns 0.
   // if character '\n' is found this returns 1.
   // this is relevant for an output function that wants
   // to print result "foo" without a preceeding linefeed.
   int   viewOffset     ( );

// private:
   int   parseFromMask  (char *pszMask);
   int   parseToMask    (char *pszMask);

   void  reset          ( );
   void  trace          (const char *pszFormat, ...);

   int   define         (uint8_t *pSrcInfo, int iCharOff, int iToken,
                         int iminlen, int imaxlen,
                         int iRecentCharClassType=0,  // or 10+actual type
                         uint8_t *pOptData=0, uint8_t *pOptFlags=0);
   int   addData        (int imask, uint8_t *pSrc, int iSrcLen, char *pOptAttr=0);
   int   classcmp       (int ipart, uint8_t *pSrc, uint8_t *aClass, int ilen);

   int   matchesSinglePoint   (int imask, uint8_t *pSrcCur, uint8_t *pSrcMax,
                               bool bLastRecord, int &rMatchLen, bool bStart,
                               int iFrom, bool bLineStart);

   void  updateBestMatch(int imask);

   char *classAsTrace   (uint8_t *aSkipMatch, int iMax);

   #ifdef SFKINT
   int   calcLength     (int imask, uint8_t iopt);
   #endif // SFKINT
   int   equalOrLowerPrio  (uint8_t ioptl, uint8_t ioptr);
   void  setPrio        (int imask, uint8_t iprio);

static int
    provideStatics      ( ),
    define              (char *pszFrom, char *pszTo),
    defineInt           (char *pszFrom, char *pszTo),
    traceLevel          ( ),
    initClass           (uint8_t *pMask, int iMaskSize, const char *pCharsIn);

static char
   *expand              (char *pszSrcIn),
   *expandSub           (char *pszSrcIn, bool bWrite, int *ppMaxDst);

static void
    safeStrCopy         (char *pszDst, const char *pszSrc, int nMaxDst);

uint8_t
  *aClFrom;
uint32_t
  *aClTo;               // with encoded part no. etc.
char
  *pszClFromText;       // a copy if below flag is set
bool
   bClIsSkipPattern,    // starts with [skip]
   bClFromTextWasCopied,
   bClToTextIsCover,    // totext is "[all]"
   bClSetKeep;          // apply [keep] in parseFromMask

int
   iClFromTok,
   iClToTok,
   iClFrom,
   iClTo,
   iClOut,
   iClBestMatch,
   iClRecentDefinedToken,
   iClRecentCharClassType, // 0:undefined 10:bytes 11:chars
   iClMaxOrInf;         // total no. of [ortext] found

uint8_t
   bClAlloc,
   bClAttr,
   bClIsValid,
   iClAppendLEnd;

uint8_t
  **aClData,   // literals and variable parts
  **aClFlags,  // bit fields marking \xnn chars
  **aClToLit,
  **aClClass;

char
  **aClAttr,   // optional color attributes
  **aClInFN;   // input length function

uint8_t
  *aClDynaClass;
   // flag per aClClass: is dynamic

uint8_t
   aClHeadMatch      [256];

int
   iClLitBuf,
   iClPartBuf,
   iClFromRange,
   iClToRange,
   iClOutFNMaxLen,
  *aiClData,         // actual used variable lengths
  *aiClBestData,     // max var lengths across all matches
  *aiClFromMinLen,   // required min length or 0
  *aiClFromMaxLen;   // declared buffer size
   // length -1 means overflow

#ifdef SFK_LOR
int
  **aClOrInf;
#endif // SFK_LOR

uint8_t
  *aClTokOpts,       // from token options
  *aClOutOpts;       // to   token options

// intermediate match storage
// to build simplified output
uint8_t
   *pClCurMatchAddr;
int
   iClCurMatchLen;

static int
   iClOff,           // off area, tolerance
   iClOutSizeMax,    // over all patterns
   iClOutSizeAlloc,  // actually alloc'ed
   iClExpBuf,        // over all patterns
   iClDefUsed,       // actual defines
   iClDefAlloc;      // size of pointer array

static uint8_t
   bClStaticInitDone,
   aClSkipMatchChars [256],
   aClSkipMatchBytes [256],
   aClSkipMatchWhite [256],
   aClSkipMatchXWhite[256],
   aClSkipMatchAlpha [256],
   aClSkipMatchAlnum [256],
   aClSkipMatchDigits[256],
   aClSkipMatchHexDig[256];

static uint8_t
  *aClOutBuf,
  *aClExpBuf,
   aClLitBuf   [SFKMATCH_MAX_LITERAL_SIZE+100],
   aClLitBufBit[(SFKMATCH_MAX_LITERAL_SIZE+100)/8], // bit field
   aClFromCopy [512],
   aClPartInfo [512],
   aClPartInfo2[512],
   aClPrioInfo2[512];

static char
  *aClOutAttr,
  **aClDefFrom,
  **aClDefTo;

static bool
   bClUseCase,
   bClXChars,
   bClExtract,
   bClXText;

static char
   cClLitAttr;
   
static int
   iClTrace;

static SFKMatchOutFNCallback_t
   pClOutFN;
};

int sfkerr(const char *pszFormat, ...);
int sfkinf(const char *pszFormat, ...);

