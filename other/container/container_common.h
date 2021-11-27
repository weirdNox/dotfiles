#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <ftw.h>
#include <glob.h>
#include <limits.h>
#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/seccomp.h>
#include <net/if.h>
#include <sched.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/prctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/statvfs.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

typedef  int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef  uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef  float r32;
typedef double r64;

typedef s8  b8;
typedef s32 b32;

typedef uintptr_t umm;
typedef  intptr_t smm;

#define stringify_(X) #X
#define stringify(X) stringify_(X)

#define macroIsEmpty(X) ((~(~X + 0) == 0) && (~(~X + 1) == 1))
#define macroValue(X) (!macroIsEmpty(X) && (X))

#define arrayCount(Arr) (sizeof(Arr)/sizeof(*(Arr)))

#define kibiBytes(X) (         (X) * 1024LL)
#define mebiBytes(X) (kibiBytes(X) * 1024LL)
#define gibiBytes(X) (mebiBytes(X) * 1024LL)

#define staticAssert(Expr) static_assert(Expr, "Assertion failed: "#Expr)

#define colorWarn(Text)  "\033[33m" Text "\x1B[0m"

#define internal static
#define global static

typedef struct {
    umm Size;
    union {
        u8 *Data;
        char *Char;
    };
} buffer;

typedef buffer string;

#define bundleArray(Array) (buffer){ .Size = sizeof(Array),    .Data = (u8 *)(Array) }
#define constZ_(String)            { .Size = sizeof(String)-1, .Data = (u8 *)(String) }
#define constZ(String)     (string)constZ_(String)

internal inline string wrapZ(char *String)
{
    string Result = {
        .Size = String ? strlen(String) : 0,
        .Data = (u8 *)String,
    };

    return Result;
}

internal inline u8 *advance(buffer *Buffer, umm Size)
{
    u8 *Result = 0;

    if(Buffer->Size >= Size)
    {
        Result = Buffer->Data;
        Buffer->Data += Size;
        Buffer->Size -= Size;
    }
    else
    {
        fprintf(stderr, "Buffer doesn't have %lu bytes available, only %lu\n", Size, Buffer->Size);
        exit(EXIT_FAILURE);
    }

    return Result;
}
#define consume(Buffer, type) ((type *)advance((Buffer), sizeof(type)))
#define consumeArray(Buffer, type, Count) ((type *)advance((Buffer), sizeof(type[Count])))

internal inline buffer getSubBuffer(buffer *Buffer, umm Size)
{
    buffer Result = {
        .Data = advance(Buffer, Size),
        .Size = Size,
    };

    return Result;
}


internal char *buildStringArgs(buffer *Buffer, char *Format, va_list ArgList)
{
    char *Result = Buffer->Char;

    int BytesWritten = vsnprintf(Buffer->Char, Buffer->Size, Format, ArgList);
    assert(BytesWritten >= 0);

    if((umm)BytesWritten < Buffer->Size)
    {
        advance(Buffer, BytesWritten);
    }
    else
    {
        advance(Buffer, Buffer->Size);
    }

    return Result;
}

internal char *buildString(buffer *Buffer, char *Format, ...)
{
    va_list ArgList;
    va_start(ArgList, Format);
    char *Result = buildStringArgs(Buffer, Format, ArgList);
    va_end(ArgList);

    return Result;
}

internal string finalizeString(buffer *Buffer, char *First)
{
    string Result = {
        .Size = (umm)(Buffer->Char - First),
        .Char = First,
    };

    char *TerminatorLocation;
    if(Buffer->Size > 0)
    {
        TerminatorLocation = (char *)advance(Buffer, 1);
    }
    else
    {
        --Result.Size;
        TerminatorLocation = Buffer->Char - 1;
    }
    *TerminatorLocation = 0;

    return Result;
}

internal string formatString(buffer *Buffer, char *Format, ...)
{
    va_list ArgList;
    va_start(ArgList, Format);
    char *First = buildStringArgs(Buffer, Format, ArgList);
    va_end(ArgList);

    string Result = finalizeString(Buffer, First);
    return Result;
}

internal inline b8 writeAll(int File, string String)
{
    b8 Result = true;

	while(String.Size)
    {
        smm BytesWrittenNow = write(File, String.Data, String.Size);

		if(BytesWrittenNow >= 0)
        {
			String.Size -= BytesWrittenNow;
            String.Data += BytesWrittenNow;
		}
        else if(errno != EINTR && errno != EAGAIN)
        {
            fprintf(stderr, "Error writing to file: %s\n", strerror(errno));
			Result = false;
            break;
        }
	}

	return Result;
}

internal inline pid_t cloneSC(u64 Flags)
{
    pid_t PID = (pid_t)syscall(SYS_clone, Flags, 0);
    return PID;
}

internal inline int pivotRootSC(char *NewRoot, char *PutOld)
{
    int Result = (int)syscall(SYS_pivot_root, NewRoot, PutOld);
    return Result;
}

internal void disableSetGroups(pid_t ChildPID)
{
    char FileName[100];
    formatString(&bundleArray(FileName), "/proc/%d/setgroups", ChildPID);

    int File = open(FileName, O_WRONLY | O_CLOEXEC);
    if(File >= 0)
    {
        if(!writeAll(File, constZ("deny")))
        {
            fprintf(stderr, "Couldn't write to %s\n", FileName);
            exit(EXIT_FAILURE);
        }

        close(File);
    }
    else
    {
        fprintf(stderr, "Could not open %s\n", FileName);
        exit(EXIT_FAILURE);
    }
}

typedef enum {
    Map_UID,
    Map_GID,
} map_type;

internal void mapID(pid_t ChildPID, map_type Type, id_t BaseID, id_t BindID)
{
    char FileName[100];
    formatString(&bundleArray(FileName), "/proc/%d/%s_map", ChildPID, Type == Map_UID ? "uid" : "gid");

    int File = open(FileName, O_WRONLY | O_CLOEXEC);
    if(File >= 0)
    {
        char Buffer[256];
        string Mapping = formatString(&bundleArray(Buffer), "%lu %lu 1\n", BindID, BaseID);

        if(!writeAll(File, Mapping))
        {
            fprintf(stderr, "Couldn't write to %s\n", FileName);
            exit(EXIT_FAILURE);
        }

        close(File);
    }
    else
    {
        fprintf(stderr, "Could not open %s\n", FileName);
        exit(EXIT_FAILURE);
    }
}

internal void keepCaps()
{
    struct __user_cap_header_struct Header = { .version = _LINUX_CAPABILITY_VERSION_3 };
    struct __user_cap_data_struct Payload[_LINUX_CAPABILITY_U32S_3] = {};

    Payload[0].permitted   = ~0;
    Payload[0].inheritable = ~0;
    Payload[0].effective   = ~0;
    Payload[1].permitted   = ~0;
    Payload[1].inheritable = ~0;
    Payload[1].effective   = ~0;

    if(capset(&Header, Payload) < 0)
    {
        fprintf(stderr, "Couldn't set capabilities\n");
        exit(EXIT_FAILURE);
    }

    for(s32 Idx = 0;; ++Idx)
    {
        if(prctl(PR_CAP_AMBIENT, PR_CAP_AMBIENT_RAISE, Idx, 0, 0) < 0)
        {
            if(errno == EINVAL)
            {
                break;
            }
            else if(errno == EPERM)
            {
                continue;
            }
            else {
                fprintf(stderr, "Couldn't raise capability\n");
                exit(EXIT_FAILURE);
            }
        }
    }
}

internal void dropCaps()
{
    struct __user_cap_header_struct Header = { .version = _LINUX_CAPABILITY_VERSION_3 };
    struct __user_cap_data_struct Payload[_LINUX_CAPABILITY_U32S_3] = {};

    if(capset(&Header, Payload) < 0)
    {
        fprintf(stderr, "Couldn't drop capabilities\n");
        exit(EXIT_FAILURE);
    }
}
