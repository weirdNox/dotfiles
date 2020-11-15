#ifndef SFKPACKIO_HPP
#define SFKPACKIO_HPP

#ifndef uint
 typedef unsigned int  uint;
 typedef unsigned long ulong;
#endif
#ifndef uchar
 typedef unsigned char uchar;
#endif
#if defined(_MSC_VER) || defined(__BORLANDC__)
 typedef unsigned __int64 sfkuint64;
#else
 typedef unsigned long long int sfkuint64;
#endif

struct SFKPackStream
{
   int    bPack;
   int    bFast;

   uchar *pin;
   int    nin;
   uchar *pout;
   int    nout;

   void  *pstream;

   char   szerr[80];
};

int sfkPackStart(SFKPackStream *p);
int sfkPackProc(SFKPackStream *p, int bLastBlock, int *bReloop);
int sfkPackEnd(SFKPackStream *p);
uint sfkPackSum(uchar *buf, uint len, uint crc);

// ----------- zip support ----------------

#ifdef FOR_SFK_INCLUDE

#define UNZ_OK                   (0)
#define UNZ_END_OF_LIST_OF_FILE  (-100)
#define UNZ_ERRNO                (Z_ERRNO)
#define UNZ_EOF                  (0)
#define UNZ_PARAMERROR           (-102)
#define UNZ_BADZIPFILE           (-103)
#define UNZ_INTERNALERROR        (-104)
#define UNZ_CRCERROR             (-105)
#define Z_ERRNO                  (-1)
#define Z_DEFLATED               8
#define Z_BZIP2ED                12
#define Z_DEFAULT_COMPRESSION    (-1)
#define Z_BEST_COMPRESSION       9
#define Z_DEFAULT_STRATEGY       0

typedef unsigned int  uInt;
typedef unsigned long uLong;
typedef void         *unzFile;

#if defined(_MSC_VER) || defined(__BORLANDC__)
 typedef unsigned __int64 ZPOS64_T;
#else
 typedef unsigned long long int ZPOS64_T;
#endif

extern "C"
{

typedef struct tm_unz_s
{
    uInt tm_sec;
    uInt tm_min;
    uInt tm_hour;
    uInt tm_mday;
    uInt tm_mon;
    uInt tm_year;
} tm_unz;

typedef struct unz_global_info64_s
{
    ZPOS64_T number_entry;
    uLong size_comment;
} unz_global_info64;

typedef struct unz_global_info_s
{
    uLong number_entry;
    uLong size_comment;
} unz_global_info;

typedef struct unz_file_info64_s
{
    uLong version;
    uLong version_needed;
    uLong flag;
    uLong compression_method;
    uLong dosDate;
    uLong crc;
    ZPOS64_T compressed_size;
    ZPOS64_T uncompressed_size;
    uLong size_filename;
    uLong size_file_extra;
    uLong size_file_comment;
    uLong disk_num_start;
    uLong internal_fa;
    uLong external_fa;
    tm_unz tmu_date;
} unz_file_info64;

typedef struct unz_file_info_s
{
    uLong version;
    uLong version_needed;
    uLong flag;
    uLong compression_method;
    uLong dosDate;
    uLong crc;
    uLong compressed_size;
    uLong uncompressed_size;
    uLong size_filename;
    uLong size_file_extra;
    uLong size_file_comment;
    uLong disk_num_start;
    uLong internal_fa;
    uLong external_fa;
    tm_unz tmu_date;
} unz_file_info;

void *zipOpen64(const void* pathname, int append);
int zipClose(void *file, const char* global_comment);

int unzGetCurrentFileInfo64 (unzFile file,
   unz_file_info64 * pfile_info,
   char * szFileName, uLong fileNameBufferSize,
   void *extraField, uLong extraFieldBufferSize,
   char* szComment,  uLong commentBufferSize);

int unzGetGlobalComment(unzFile file, char *szComment, uLong uSizeBuf);

int unzOpenCurrentFilePassword (unzFile file, const char*  password);
int unzReadCurrentFile  (unzFile file, void *buf, unsigned len);
int unzCloseCurrentFile (unzFile file);
unzFile unzOpen64 (const void *path);

int unzGetGlobalInfo64 (unzFile file, unz_global_info64* pglobal_info);
int unzGetGlobalInfo (unzFile file, unz_global_info* pglobal_info32);

int unzGoToNextFile (unzFile  file);
int unzClose (unzFile file);
}

#endif // FOR_SFK_INCLUDE

#endif
