
#ifdef VFILEBASE


#ifdef USE_DCACHE

// area one, ordered by namesum
struct DMetaEntry {
   num   sumhi;
   num   sumlo;
   num   size;
   num   time;
};

// area two, ordered by fifo
struct DFifoEntry {
   num   sumhi;
   num   sumlo;
};

#endif // USE_DCACHE

// map of managed keys and MANAGED Cois.
// entries can also be read in a fifo way.
class CoiMap : public KeyMap {
public:
      CoiMap   ( );
     ~CoiMap   ( );

   void  reset (bool bWithDiskCache, const char *pszFromInfo);
   // expects: all coi entries have zero references.
   // if not, errors are dumped.
   // deletes all coi entries.

   int  put   (char *pkey, Coi *pTransferCoi, const char *pTraceFrom, int nmode=0);
   // enter the Coi under that key.
   // the Coi is NOT COPIED.
   // Coi ownership is TRANSFERED to the CoiMap.
   // the Coi may be DELETED anytime if it's
   // reference counter reaches zero.

   Coi *get    (char *pkey);
   // CALLER MUST RELEASE (decref) after use!
   // if a value was stored then it is returned.
   // the Coi is NOT COPIED.
   // if not, null is returned, no matter if the key is set.

   int  remove(char *pkey);
   // remove entry with that key, if any.
   // associated Coi must have zero refs.
   // associated Coi is deleted.
   // rc: 0:done 1:no_such_key >=5:error.

   num   byteSize(bool bCalcNow=0);
   // approximate added bytesize of all objects.
   // calcnow forces immediate, expensive calculation.

   num   bytesMax       ( );  // peak memory use
   num   filesDropped   ( );  // files dropped during processing

   int  tellByteSizeChange(Coi *pcoi, num nOldSize, num nNewSize);
   // if pcoi is in the cache, this method quickly
   // adapts the overall byte count.

protected:
   int  putDiskCache(char *pkey, Coi *pcoi, const char *pTraceFrom);
   int   bfindDMeta(num nsumlo,num nsumhi,int &rindex);

   List  clFifo;
   num   nClByteSize;   // approximately
   num   nClBytesMax;   // peak memory use
   num   nClDropped;    // files dropped during processing

   #ifdef USE_DCACHE
   int        nClDAlloc;
   int        nClDUsed;
   DMetaEntry *apClDMeta;
   DFifoEntry *apClDFifo;
   #endif // USE_DCACHE
};

class TCPCore;
class Coi;
class CoiTable;

// optional class: derive own objects from this
// and register with TCPCon's to associate own
// data with connections.
class TCPConData
{
public:
            TCPConData  ( );
   virtual ~TCPConData  ( ) = 0;
};

class TCPCon
{
public:
      TCPCon   (SOCKET hsock, TCPCore *pcorein, int nTraceLine=0);
     ~TCPCon   ( );

   // ===== data I/O functions =====
   void  setBlocking (bool bYesNo);

   int  read     (uchar *pblock, uint nlen, bool bReturnAny=0);
   // read raw block of bytes. this will block
   // until nlen bytes are read, or connection is closed.
   // returns <= 0 on connection close, else nlen.
   
   int  send     (uchar *pBlock, uint nLen);

   char *readLine (char *poptbuf=0, uint noptmaxbuf=0, bool braw=0);
   // read()'s char by char until CRLF is reached.
   // returns supplied buffer, or NULL on EOD/close.
   // if no buffer is supplied, provides internal buffer.
   // returned lines are stripped from CRLF except if braw.
   // SHARE: may share buffer with putf.

   // raw text I/O, sends all CRLF as supplied by caller:
   int  puts  (char *pline);
   int  putf  (cchar *pmask, ...);
   // SHARE: may share buffer with readLine.

   // ftp and http protocol support
   int  setProtocol (char *psz);   // "ftp", "http"
   int  readReply   (int nMinRC, int nMaxRC);
   // reads a reply like "200 OK", "HTTP/1.1 200 OK",
   // checks RC if it's within range, returns 0 if so.


// private:
   TCPCore &core  ( );
   void  rawClose ( );
   char *buffer   (int &rbufsize);

   TCPCore     *pClCore;
   SOCKET       clSock;
   TCPConData  *pClUserData;  // optional
   char        *pClIOBuf;     // alloc'ed on demand
   static int  nClIOBufSize;  // if it is alloc'ed

   int   nClTraceLine;
   num   nClStartTime;
   int   iClMaxWait;
   int   iClPort;

   struct sockaddr_in clFromAddr;


friend class TCPCore;
};

// basic tcp i/o support.
class TCPCore
{
public:
      TCPCore  (char *pszID, char cProtocol);
     ~TCPCore  ( );

   static TCPCore &any( ); // generic core for scripting

   char  *getID   ( );

   // windows only:
   static int sysInit  ( );
   // run TCPCore::sysInit() before any tcp transfer

   static void sysCleanup  ( );
   // run this last before exiting the application

   void  shutdown ( );
   // close everything, delete all user objects
   // associated with connections (if any).
   // windows only: if bSysCleanup is set, runs also
   // WSACleanup() to cleanup the windows TCP stack.

   int makeServerSocket (int nport, TCPCon **ppout, bool bquiet=0);
   // returned object is managed by TPCCore.
   // returned server socket is added to readfds set.

   int accept (TCPCon *pServerCon, TCPCon **ppout);
   // returned object is managed by TPCCore.
   // returned client socket is added to core's readfds set.

   int connect(char *phostorip, int nport, TCPCon **ppout, bool bSSL=0);
   // returned object is managed by TPCCore.
   // client socket is added to readfds set.

   int  close    (TCPCon *pcon);
   // closes connection, deletes associated objects, if any.
   // DELETES supplied pcon.

   int  lastError   ( );
   // system errno of last operation. depends on call if it's set.

   // for a centralized read and processing thread:
   int  selectInput (TCPCon **pNextActiveCon, TCPCon *pToQuery=0, int iMaxWaitMSec=0);
   // from all sockets pending input, returns the first one
   // that received an accept/connect or read event.
   // if pToQuery is given, returns that pointer and rc0
   // if exactly that connection has input pending.
   // rc0=OK_active_socket_set rc1=nothing_to_do >=5:fatal error

   // optional: register a socket with a user-supplied object
   // derived from TCPConData.
   // -  the object must be created by the user through "new".
   // -  it is deleted by SFKTCP on close() or shutdown().
   int  registerData(TCPCon *pcon, TCPConData *pUserData);

   void  setVerbose  (bool bYesNo);
   // dump sent/received control lines to terminal

   bool  verbose     ( );

   // reference counting:
   int  incref   (cchar *pTraceFrom);  // increment refcnt
   int  decref   (cchar *pTraceFrom);  // decrement refcnt
   int  refcnt   ( );  // current no. of refs

   // close all open connections:
   void  closeConnections  ( );

   // set basic proxy infos, may be used by derived classes only.
   static int    setProxy (char *phost, int nport);

// protected:
   int  checksys ( );
   void  wipe     ( );
   int  addCon   (TCPCon *pcon);   // add to fdset
   int  remCon   (TCPCon *pcon);   // remove from fdset

   char   *pClID;
   int    nClCon;     // must be < FD_SETSIZE
   int    nClMaxSock;   // important for select

   // TODO: check sizes of these. maybe make them dynamic ptrs,
   //       to assure a tcp core still suits on a stack.
   TCPCon *aClCon[FD_SETSIZE+2];
   fd_set  clReadSet; // all sockets pending accept and read
   fd_set  clSetCopy;
   fd_set  clSetCopyW;
   fd_set  clSetCopyE;
   bool    bClVerbose;
   int     nClRefs;
   int     iClMaxWait;
   char    cClProtocol;

   static  char szClProxyHost[100];
   static  int  nClProxyPort;
   static  bool  bClProxySet;
   static  bool bSysInitDone;

   static TCPCore *pClGeneric;   // sfk1972
};

// class to auto close a connection,
// AND to automatically clear its pointer.
class ConAutoClose {
public:
      ConAutoClose   (TCPCore *pcore, TCPCon **pcon, int nTraceLine);
     ~ConAutoClose   ( );
private:
      int     nClTraceLine;
      TCPCore  *pClCore;
      TCPCon  **ppClCon;
};

class CharAutoDelete {
public:
      CharAutoDelete (char **pdata) { ppClPtr = pdata; }
     ~CharAutoDelete ( ) { if (*ppClPtr) delete [] *ppClPtr; }
private:
      char **ppClPtr;
};

// TODO: rearrange logic?
// -  one ftpclient instance per host
// -  this ftpclient must remember the ftp://hostname
// -  then list() returns absolute names based on this
// UNCLEAR: when to logout on such a client
class FTPClient : public TCPCore
{
public:
      FTPClient(char *pszID);
     ~FTPClient( );

   int   login (char *phostorip, int nport=21);
   // anonymous ftp only, so far.

   int   loginOnDemand(char *phostorip, int nport);
   // does nothing if already logged in

   void   logout( );

   int   openFile   (char *pfilename, cchar *pmode);
   // begin download of a file with a relative path,
   // e.g. subdir/thefile.txt

   int   readFile   (uchar *pbuf, int nbufsize);
   // read block from currently open file

   void   closeFile  ( );
   // close currently open fle

   int   list (char *pdir, CoiTable **pptable, char *pRootURL=0);
   // NOTE: if pptable is the address of a NULL pointer,
   //       a CoiTable will be created and returned.
   // NOTE: if pptable points to an existing CoiTable,
   //       file and dir entries will be filled therein.
   // returned CoiTable is always managed by caller.
   // RootURL: if supplied, every filename is prefixed by that,
   //          e.g. "ftp://thehost/thedir/"

   int  setPassive  (TCPCon **ppout);

   char  *line    ( );  // line i/o buffer
   int   lineMax ( );  // max size of that buffer

private:
   int  readLine (TCPCon *pcon);
   int  readLong (TCPCon *pcon, uint &rOut, cchar *pszInfo);
   int  sendLong (TCPCon *pcon, uint nOut, cchar *pszInfo);
   int  readNum  (TCPCon *pcon, num &rOut, cchar *pszInfo);
   int  readNum  (uchar *pbuf, int &roff, num &rOut, cchar *pszInfo);
   int  sendNum  (TCPCon *pcon, num nOut, cchar *pszInfo);

   void  wipe     ( );
   int  splitURL (char *purl);
   char *curhost  ( );  // from spliturl
   char *curpath  ( );  // from spliturl
   int  curport  ( );  // from spliturl
   int  sendLine (char *pline);
   char *readLine ( );
   int  readReply(char **ppline);

   TCPCon   *pClCtlCon;        // control connection
   TCPCon   *pClDatCon;        // data connection, if separate
   char      aClURLBuf[300];   // current hostname, path
   char     szClLineBuf[1000]; // for file i/o
   int      nClPort;          // default is 21
   char     *pClCurPath;       // within URLBuf

   bool      bClLoggedIn;
   bool      bClSFT;
   int      nClSFTVer;
   char     *pszClHost;

   struct   CurrentIOFile {
      char   *name;
      bool    blockmode;   // else bulk
      int    block;       // read block number
      num     time;
      num     size;
      num     remain;
      uchar   abmd5[20];
      SFKMD5 *md5;
   }  file;

   // reply string to PASV. if set, we're in passive mode,
   // receiving one or more downloads.
   char     *pClPassive;

friend class Coi;
};

class HTTPClient : public TCPCore
{
public:
      HTTPClient(char *pszID);
     ~HTTPClient( );

   int   open (char *purl, cchar *pmode, Coi *pcoi);
   // begin download from an url.
   // NOTE: pcoi's name might be changed on redirect.

   int   read (uchar *pbuf, int nbufsize);
   // read block from currently open url stream.
   // chunked transfer is managed transparently.

   void   resetCache( );
   // use between access to two url's.

   void   close( );
   // close currently open url.

   int   getFileHead   (char *purl, Coi *pout, cchar *pinfo="");
   // read header infos like content-type and size.
   // the provided Coi is filled.

   int   list (char *purl, CoiTable **pptable);
   // NOTE: if pptable is the address of a NULL pointer,
   //       a CoiTable will be created and returned.
   // NOTE: if pptable points to an existing CoiTable,
   //       file and dir entries will be filled therein.
   // returned CoiTable is always managed by caller.

   int  sendReq  (cchar *pcmd, char *pfile, char *phost, int nport);
   // requires an open connection

   int  connectHttp    (char *phostorip, int nport, TCPCon **ppout);
   // returned object is managed by TPCCore.
   // same as connect() but with optional proxy handling.

   int  iClWebRC;

private:
   bool  haveConnection ( );
   void  wipe     ( );

   int  readraw      (uchar *pbuf, int nbufsize);
   int  readrawfull  (uchar *pbuf, uint nbufsize);
   // read block without any chunk handling.

   int  splitURL  (char *purl);
   char *curhost  ( );  // from spliturl
   char *curpath  ( );  // from spliturl

   int  joinURL   (char *pabs, char *pref);
   char *curjoin  ( );  // from joinurl

   int  splitHeader (char *phead);
   char *curname  ( );  // of header entry
   char *curval   ( );  // of header entry

   int  sendLine  (char *pline);
   char *readLine (bool braw=0);

   int  rawReadHeaders (int &rwebrc, char *purlinfo, Coi *pOptCoi=0); // , StringMap *pheads=0);
   // returns sfk rc, 0 (OK) ... 9 (fatal error)
   // returns also webrc, e.g. 200 (OK) or 302 (redirect)

   int      nClPort;

   TCPCon   *pClCurCon; // current i/o connection

   char      aClURLBuf[500];  // current hostname, path
   char     *pClCurPath;      // within URLBuf

   char      aClHeadBuf[200]; // current header line
   char     *pClCurVal;       // within HeadBuf

   char      aClJoinBuf[500]; // join: recombined url

   char      aClIOBuf[4096];

   // Header-Free Servers: first reply line cache
   char     *pszClLineCache;  // only for first line

   // Transfer-Encoding: chunked support
   bool      bClChunked;
   uchar    *pClCache;        // alloc'ed on demand
   int      nClCacheAlloc;
   int      nClCacheUsed;    // remaining unread bytes

   // is http server blocking head requests?
   bool      bClFirstReq;
   bool      bClNoHead;

   // if set, no connections are possible:
   bool      bClNoCon;

   bool      bClSSL;


friend class Coi;
};

class ConCache : public KeyMap
{
public:
      ConCache   ( );
     ~ConCache   ( );

      // creates clients on demand. returned clients
      // - are still MANAGED BY THE CACHE
      // - must be released after use
      #ifdef USE_WEBCONCACHE
      HTTPClient  *allocHttpClient(char *pBaseURL);
      #endif
      FTPClient   *allocFtpClient (char *pBaseURL);
      // returns NULL if no locked con is available

      // must be called after temporary use,
      // e.g. download of a single web file:
      #ifdef USE_WEBCONCACHE
      int  releaseClient (HTTPClient *p);
      #endif
      int  releaseClient (FTPClient  *p);

      void  closeConnections  ( );
      void  reset (bool bfinal, const char *pszFromInfo);

private:
      int   closeAndDelete(void *praw);
      // caller MUST remove() manually after that!

      int  forceCacheLimit( );

      // to remember fifo sequence:
      // fifo list with managed key strings
      num       nAddCnt;
      StringMap clFifo;
};

// implemented in sfknet.cpp, cleanup in sfk.cpp:
extern ConCache glblConCache;
extern CoiMap glblVCache;
extern KeyMap glblCircleMap;
extern KeyMap glblOutFileMap;

#endif // VFILEBASE



/*
   sfktxt protocol fields:
      v100  :  version 1.0.0
      reqn  :  request number n, with n >= 1
      rtn   :  retry number n, 0 to 3
      copy  :  request a reply for request
      repn  :  reply to request number n
      cs1   :  text is encoded using color scheme 1:
               \x1F + color code: rgbymc (dark) RGBYMC (bright) 
                  wW = white  , viewers with white background may use gray 
                  d  = default, viewers with white background will use black
               if \x1F was found in original input, it is escaped as \x1F\x1F
      scr   :  start color is 'r'ed for this message
      fl    :  finished line, last line of packet is complete
      sl    :  split line, last line continues in next packet
      
   sfktxt conventions:
      -  if sender sends ,copy the receiver must reply immediately
         with a ,rep record. otherwise, the line is broken.
      -  as long as no CR or LF is received, text must be joined into
         one large line. typical maximum line length is about 4000 chars.
         large lines spanning multiple packets are also marked by ,sl.
      -  if ,sc is found receiver should set initial color to that value.

   example message:

      data (\n == LF, \x1F == character with code 0x1F)
         :sfktxt:v100,req1,rt0,cs1,scR,fl\n
         :clear\n
         \n
         \x1FRfoo\x1Fd and \x1Fbbar\n

      remarks
         -  req1   : the first request from this client
         -  rt0    : this is the original message, no retries yet
         -  cs1    : uses color coded text with 0x1F tags
         -  scR    : start color is bright 'R'ed
         -  fl     : last line is finished, no spanning of messages
         -  :clear : viewer should clear the log then add text
         -  there is no ",copy" field, so sender expects no reply
         -  \x1FRfoo\x1Fd and \x1Fbbar\n
            print "foo" in bright red, then 'd'efault color etc.
*/

#define UDPIO_MAX_CLIENTS 128

class UDPIO
{
public:
      UDPIO    ( );
     ~UDPIO    ( );

void
      rawInit  ( );

int
      initSendReceive   (const char *pszDescription,
                         int iOwnReceivePort,
                         // provide -1 to allocate any port from system
                         int iTargetSendPort,
                         char *pszTargetHostname,
                         // provide a single machine's IP for Unicast.
                         // provide 224.0.0.x to use Multicast.
                         // provide NULL to use a Unicast IP later.
                         uint uiFlags=0
                         // 1 : force multicast
                         // 2 : reuse address
                         // 4 : retry on alternative ports
                        ),
      setTarget         (char *pszHostname, int iPort),

      // --------- raw I/O ---------
      sendData          (uchar *pData, int iDataSize),
      receiveData       (uchar *pBuffer, int iBufferSize,
                         struct sockaddr_in *pAddrIncoming=0,
                         int iSizeOfAddrIncoming=0
                        ),
      closeAll          ( );

bool
      isOpen            ( ),
      isMulticast       ( ),
      isDataAvailable   (int iSec=0, int iMSec=0);

      // -------- network text I/O ----------
int   // RC 0 == OK
      addCommand        (char *pszCmd), // call before sendText
      addOrSendText     (char *pszText, char *pszAttr),
      addOrSendText     (char *pszPhrase, int iPhraseLen, bool bNoWrap),
      flushSend         (bool bTellAboutSplitLine),
      addHeader         ( ),
      receiveText       ( ),  // MUST call getNextInput next
      storeHeader       (char *pszRaw, int iHeadLen),
      sendDuplexReply   (struct sockaddr_in *pTo),
      decodeColorText   (int iFromOffset),
      checkTellCurrentColor (char c),
      getClientIndex    (struct sockaddr_in *pAddr, bool *pFound),
      getTextSendDelay  ( );  // depending on (non)duplex mode
bool
      hasCachedInput    ( ),
      hasCachedOutput  ( );
char
     *getNextCommand    ( ),  // NULL if none
     *getNextInput      (char **ppAttr=0,
                         struct sockaddr_in *pSenderAddr=0,
                         bool bDontCache=0), 
                        // returns NULL if none
     *peekHeader        (char *pszField),
      sfkToNetColor     (char c);

char
      szClDescription   [30];
int
      iClOwnReceivePort,
      iClTargetSendPort,
      iClTimeout,
      iClNonDuplexSendDelay,
      fdClSocket;
bool
      bClMulticast,
      bClVerbose,
      bClRawText,
      bClCmdClear,
      bClDuplex,
      bClColor,
      bClCopyRequest,
      bClContinuedStream,
      bClForceNextInput,
      bClAppendLFOnRaw,
      bClDecodeColor,
      bClIPWasExpanded;

char
      cClTellColor,
      cClCurrentInColor;

struct sockaddr_in 
      clTargetAddr,
      clRawInAddr,      // of current received package
      clInBufInAddr;    // for start of text line
char  
      aClHeaderBuf      [1000+100],
      aClCommand        [10][100],
      aClRawOutBuf      [2500+100],
      aClRawInBuf1      [2500+100],
      aClRawInBuf2      [2500+100],
      aClRawInAttr2     [2500+100],
      aClInBuf          [MAX_LINE_LEN+1000],
      aClInAtt          [MAX_LINE_LEN+1000];
int
      iClPackageSize,
      iClCommand,
      iClHeadSize,
      iClOutIndex,
      iClInBufUsed,
      iClRawInputCached,
      iClReqNum,
      iClRecentReqNum,
      iClInReqNum,
      iClInRetryNum,
      iClRetryOff,      // retry number offset in header
      iClSLIOff,        // split line indicator offset in header
      iClClient,        // current client index
      iClUsingAltPortInsteadOf;  // if bind on selected port failed,
      // alternative port is used, and failed port is flagged here.

struct UCPClientState
{
   num    ntime;        // if 0, slot is empty
   int    reqnum;       // most recent received reqnum
   struct sockaddr_in addr;
   int    copyreq;      // recent reqnum of copy answer
   int    copytry;      // recent trynum of copy answer
   char   color;        // text color at end of recent record
}
   aClClients  [UDPIO_MAX_CLIENTS+10];
};

#ifndef USE_SFK_BASE
extern UDPIO sfkNetSrc;
#endif // USE_SFK_BASE

extern int netErrno();
extern char *netErrStr();
extern void printSamp(int nlang, char *pszOutFile, char *pszClassName, int bWriteFile, int iSystem);

#define MAX_UDP_CON (mymin(300, FD_SETSIZE))

struct UDPCon
{
   char     host[128];
   char     ipstr[128];
   char     reply[128];
   int      port;
   struct   sockaddr_in addr;
   SOCKET   sock;
   num      tsent;
   int      idelay;
   int      replylen;
   int      istate;
};

class UDPCore
{
public:
      UDPCore  (int iMaxWait);
     ~UDPCore  ( );

   int   makeSocket  (int iMode, char *phost, int nport);
   void  shutdown    ( );

   int   selectInput (int *pIndex, int iMaxWaitMSec);
   int   lastError   ( );

   void  stepPing    (int i);
   int   sendPing    (int i);
   int   recvPing    (int i, int *pDelay);

   UDPCon   aClCon[MAX_UDP_CON+2];
   int      nClCon;    // must be < FD_SETSIZE
   int      nClMaxSock;
   int      iClResponses;

   fd_set   clReadSet; // all sockets pending accept and read
   fd_set   clSetCopy;

   int   bverbose;
   int   bpure;
   num   tstart;
   int   imaxwait;
};

class AlignTest1 { public: int a1; char c1; };
class AlignTest2 { public: int a1; char c1; char c2; };
class AlignTest3 { public: int a1; char c1; char c2; char c3; };

extern void getAlignSizes1(int &n1, int &n2, int &n3);
extern void getAlignSizes2(int &n1, int &n2, int &n3);
