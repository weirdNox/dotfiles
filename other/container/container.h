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


#define BaseRootFolder "/base"
#define BindRootFolder "/bind"
global string HelperBinds[] = { HELPER_BINDS(constZ_) };

#define MountPointMaximumLength 255

#define CONFIGURE_CONTAINER() internal void configureContainer()
#define RUN_COMMAND() internal void runCommand(int ArgCount, char *ArgVals[])


global pid_t ParentPID;
global char *InitialWorkingDirectory;

global char *AuxDirectory;

global uid_t BaseUID;
global gid_t BaseGID;
global char  BaseUserName[64];
global char *BaseHomePath;

global char *BaseSystDBus;
global char *BaseUserDBus;

global char *BaseXAuth;

global char *BaseUnionFS;

global char *BaseTTY;
global int ProcFD;

global b32 FullBind;


internal string formatString(buffer *Buffer, char *Format, ...)
{
    va_list ArgList;

    va_start(ArgList, Format);
    int BytesWritten = vsnprintf((char *)Buffer->Data, Buffer->Size, Format, ArgList);
    va_end(ArgList);

    assert(BytesWritten >= 0);

    string Result = { .Data = Buffer->Data };

    if((umm)BytesWritten < Buffer->Size)
    {
        Result.Size = BytesWritten;
    }
    else
    {
        fprintf(stderr, "String didn't fit buffer: Needed %d, only had %lu\n", BytesWritten, Buffer->Size);
        exit(EXIT_FAILURE);
    }

    umm UsedBytes = Result.Size+1;
    advance(Buffer, UsedBytes);

    return Result;
}

internal inline uid_t getBindUID()
{
    uid_t Result;
#if macroIsEmpty(BIND_UID)
    Result = BaseUID;
#else
    Result = BIND_UID;
#endif
    return Result;
}

internal inline gid_t getBindGID()
{
    gid_t Result;
#if macroIsEmpty(BIND_GID)
    Result = getBindUID();
#else
    Result = BIND_GID;
#endif
    return Result;
}

internal inline char *getBindUserName()
{
    char *Result;
#if macroIsEmpty(BIND_USER_NAME)
    Result = BaseUserName;
#else
    Result = stringify(BIND_USER_NAME);
#endif
    return Result;
}

internal inline char *getBindHomePath(buffer *Buffer)
{
    char *Result;

    string HomePath = constZ(stringify(BIND_HOME_PATH)"");
    if(HomePath.Size == 0)
    {
        HomePath = formatString(Buffer, "/home/%s", getBindUserName());
    }

    Result = (char *)HomePath.Data;

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

typedef enum {
    Node_NoExist,
    Node_File,
    Node_Directory,
    Node_Link,
    Node_Other,
} node_type;

internal node_type getNodeType(string Path, b8 FollowLinks)
{
    node_type Result = Node_NoExist;

    struct stat Stat;
    if((FollowLinks ? stat : lstat)((char *)Path.Data, &Stat) == 0)
    {
        Result = (S_ISREG(Stat.st_mode) ? Node_File :
                  S_ISDIR(Stat.st_mode) ? Node_Directory :
                  S_ISLNK(Stat.st_mode) ? Node_Link : Node_Other);
    }

    return Result;
}

internal b8 makeDirectoryRaw_(string Path, mode_t Mode)
{
    b8 Result = true;

    char Buffer[1<<13];

    if(Path.Size + 1 <= sizeof(Buffer))
    {
        memcpy(Buffer, Path.Data, Path.Size);
        Buffer[Path.Size] = 0;

        char *Iter = Buffer;

        while(Iter)
        {
            while(*Iter == '/') {
                ++Iter;
            }

            while(*Iter && *Iter != '/') {
                ++Iter;
            }

            if(*Iter == 0) {
                Iter = 0;
            }
            else {
                *Iter = 0;
            }

            switch(getNodeType(wrapZ(Buffer), false))
            {
                case Node_Directory: {} break;

                case Node_NoExist:
                {
                    if(mkdir((char *)Buffer, Mode) == -1 && errno != EEXIST)
                    {
                        Result = false;
                        Iter = 0;
                    }
                } break;

                default: {
                    fprintf(stderr, "Node %s already exists and isn't a directory\n", Buffer);
                    exit(EXIT_FAILURE);
                } break;
            }

            if(Iter) {
                *(Iter++) = '/';
            }
        }
    }
    else
    {
        fprintf(stderr, "Path doesn't fit in buffer\n");
        exit(EXIT_FAILURE);
    }

    return Result;
}

internal void makeDirectoryRaw(string Path, mode_t Mode)
{
    if(!makeDirectoryRaw_(Path, Mode))
    {
        fprintf(stderr, "Failed to create directory %s: %s\n", Path.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal void makeDirectory(char *Path, mode_t Mode)
{
    assert(Path && Path[0] == '/');

    u8 Memory[1<<13] = {};
    string PivotedPath = formatString(&bundleArray(Memory), BindRootFolder"%s", Path);

    makeDirectoryRaw(PivotedPath, Mode);
}

internal inline string getDirectory(string Path)
{
    string Result = { .Data = Path.Data };

    umm InitialSize = Path.Size;

    while(Path.Size > 1)
    {
        if(Path.Data[0] == '/')
        {
            Result.Size = InitialSize - Path.Size;
        }
        advance(&Path, 1);
    }

    return Result;
}

internal b8 createFileRaw_(string Path, mode_t Mode)
{
    b8 Result = true;

    switch(getNodeType(Path, false))
    {
        // NOTE(nox): This function is currently used to create bind mount targets, and it looks like
        // bind mounts can be made over non-regular files!
        case Node_Other: case Node_File: {} break;

        case Node_NoExist:
        {
            string Directory = getDirectory(Path);
            makeDirectoryRaw(Directory, 0755);

            int File = creat((char *)Path.Data, Mode);
            if(File < 0)
            {
                Result = false;
            }
            else
            {
                close(File);
            }
        } break;

        default: {
            fprintf(stderr, "Node %s already exists and isn't a file\n", Path.Data);
            exit(EXIT_FAILURE);
        } break;
    }

    return Result;
}

internal inline void createFileRaw(string Path, mode_t Mode)
{
    if(!createFileRaw_(Path, Mode))
    {
        fprintf(stderr, "Could not create file %s: %s\n", Path.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal void symbolicLinkRaw(string Target, string LinkPath)
{
    string Directory = getDirectory(LinkPath);
    makeDirectoryRaw(Directory, 0755);

    switch(getNodeType(LinkPath, false))
    {
        case Node_NoExist: break;

        case Node_Link: {
            unlink((char *)LinkPath.Data);
        } break;

        default: {
            fprintf(stderr, "Node %s already exists and isn't a symbolic link\n", LinkPath.Data);
            exit(EXIT_FAILURE);
        } break;
    }

    if(symlink((char *)Target.Data, (char *)LinkPath.Data) < 0)
    {
        fprintf(stderr, "Could not create symlink %s (-> %s): %s\n", LinkPath.Data, Target.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal void symbolicLink(char *Target, char *LinkPath)
{
    assert(Target);
    assert(LinkPath && LinkPath[0] == '/');

    u8 Memory[1<<13] = {};
    string PivotedLinkPath = formatString(&bundleArray(Memory), BindRootFolder"%s", LinkPath);

    symbolicLinkRaw(wrapZ(Target), PivotedLinkPath);
}

internal string linkTargetRaw(buffer *Buffer, string LinkPath)
{
    assert(LinkPath.Data && LinkPath.Data[0] == '/');

    smm TargetPathSize = readlink((char *)LinkPath.Data, (char *)Buffer->Data, Buffer->Size);
    if(TargetPathSize < 0)
    {
        fprintf(stderr, "Could not read link %s target: %s\n", (char *)LinkPath.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }

    string Result = getSubBuffer(Buffer, TargetPathSize+1);
    Result.Data[Result.Size-1] = 0;

    return Result;
}

internal int deletePathCallback_(const char *Path, const struct stat *Stat, int TypeFlag, struct FTW *FTW)
{
    if(remove(Path) != 0)
    {
        fprintf(stderr, "Couldn't remove %s\n", Path);
        exit(EXIT_FAILURE);
    }

    return 0;
}

internal void deletePathRaw(char *Path)
{
    if(nftw(Path, deletePathCallback_, 64, FTW_DEPTH | FTW_PHYS) != 0)
    {
        fprintf(stderr, "Couldn't delete %s\n", Path);
        exit(EXIT_FAILURE);
    }
}

internal inline pid_t cloneSC(u64 Flags)
{
    pid_t PID = syscall(SYS_clone, Flags, 0);
    return PID;
}

internal inline int pivotRootSC(char *NewRoot, char *PutOld)
{
    int Result = syscall(SYS_pivot_root, NewRoot, PutOld);
    return Result;
}

internal inline void createAuxDirectory()
{
    u8 Memory[1<<10];
    string AuxDirStr = formatString(&bundleArray(Memory), "/tmp/container-%lu", ParentPID);
    makeDirectoryRaw(AuxDirStr, 0755);

    AuxDirectory = strdup((char *)AuxDirStr.Data);
}

internal inline string auxNode(buffer *Buffer, char *NodePath)
{
    assert(NodePath);

    string Result = formatString(Buffer, "%s/%s", AuxDirectory, NodePath);
    return Result;
}

internal inline void dieWithParent()
{
    if(prctl(PR_SET_PDEATHSIG, SIGKILL) < 0)
    {
        fprintf(stderr, "Could not set parent death signal\n");
        exit(EXIT_FAILURE);
    }
}

internal pid_t runProgramInBackground(char *ShellArgs[])
{
    pid_t ProcPID = fork();
    if(ProcPID == 0)
    {
        // NOTE(nox): Child
        dieWithParent();
        if(execvp(ShellArgs[0], ShellArgs) < 0)
        {
            fprintf(stderr, "Couldn't execute '%s': %s\n", ShellArgs[0], strerror(errno));
            exit(EXIT_FAILURE);
        }
    }
    else if(ProcPID < 0)
    {
        fprintf(stderr, "Couldn't fork to run program '%s' in background", ShellArgs[0]);
        exit(EXIT_FAILURE);
    }

    return ProcPID;
}

internal inline void runProgram(char *ShellArgs[])
{
    pid_t ProcPID = runProgramInBackground(ShellArgs);
    waitpid(ProcPID, 0, 0);
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

internal void createUnionFS()
{
#if defined(UNIONFS_CREATE) && UNIONFS_CREATE
    u8 Memory[1<<13];
    buffer Buffer = bundleArray(Memory);

    string Upper = constZ(UNIONFS_UPPER"").Size ? constZ(UNIONFS_UPPER"") : auxNode(&Buffer, "unionfs_upper");
    string Merge = auxNode(&Buffer, "unionfs_merge");
    BaseUnionFS = strdup((char *)Merge.Data);

    makeDirectoryRaw(Upper, 0755);
    makeDirectoryRaw(Merge, 0755);

    char *ShellArgs[] = {
        "unionfs",
        "-o", "cow,relaxed_permissions",
        (char *)formatString(&Buffer, "%s=RW:/=RO", Upper.Data).Data,
        (char *)Merge.Data,

        0
    };
    runProgram(ShellArgs);
#endif
}

internal inline void unmountUnionFS()
{
#if defined(UNIONFS_CREATE) && UNIONFS_CREATE
    char *ShellArgs[] = {
        "umount",
        BaseUnionFS,

        0
    };
    runProgram(ShellArgs);
#endif
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
#if defined(DROP_CAPS) && DROP_CAPS
    struct __user_cap_header_struct Header = { .version = _LINUX_CAPABILITY_VERSION_3 };
    struct __user_cap_data_struct Payload[_LINUX_CAPABILITY_U32S_3] = {};

    if(capset(&Header, Payload) < 0)
    {
        fprintf(stderr, "Couldn't drop capabilities\n");
        exit(EXIT_FAILURE);
    }
#endif
}

typedef enum {
    Bind_Dev        = 1<<1,
    Bind_ReadOnly   = 1<<2,
    Bind_Try        = 1<<3,
    Bind_EnsureDir  = 1<<4,
    Bind_EnsureFile = 1<<5,
    Bind_KeepLinks  = 1<<6
} bind_option;

internal inline u64 getExtraMountFlags(bind_option Options)
{
    u64 Result = (((Options & Bind_Dev)      ? 0         : MS_NODEV) |
                  ((Options & Bind_ReadOnly) ? MS_RDONLY : 0       ));

    return Result;
}

internal inline void bindRemount(string Point, bind_option Options)
{
    struct statvfs FileSystemInfo;
    if(statvfs((char *)Point.Data, &FileSystemInfo) < 0)
    {
        fprintf(stderr, "Could not stat filesystem at %s: %s\n", Point.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }

    u64 Flags = FileSystemInfo.f_flag | MS_SILENT | MS_BIND | MS_REMOUNT | getExtraMountFlags(Options);

    if(mount(0, (char *)Point.Data, 0, Flags, 0) < 0)
    {
        fprintf(stderr, "Could not remount %s: %s\n", Point.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal void bindMountRaw(string PivotedBase, string PivotedBind, bind_option Options)
{
    if(Options & Bind_EnsureDir)
    {
        makeDirectoryRaw(PivotedBase, 0755);
    }
    else if(Options & Bind_EnsureFile)
    {
        createFileRaw(PivotedBase, 0444);
    }

    node_type BaseType = getNodeType(PivotedBase, (Options & Bind_KeepLinks) ? false : true);

    if(BaseType == Node_NoExist)
    {
        if(!(Options & Bind_Try))
        {
            fprintf(stderr, "Could not stat required file %s: %s\n", PivotedBase.Data, strerror(errno));
            exit(EXIT_FAILURE);
        }

        return;
    }

    typedef enum {
        BindOp_Normal,
        BindOp_Directory,
        BindOp_Link,
    } bind_op;

    bind_op Op = ((BaseType == Node_Link)      ? BindOp_Link :
                  (BaseType == Node_Directory) ? BindOp_Directory : BindOp_Normal);

    if(Op == BindOp_Link)
    {
        u8 Memory[1<<10];
        string Target = linkTargetRaw(&bundleArray(Memory), PivotedBase);
        symbolicLinkRaw(Target, PivotedBind);
    }
    else {
        if((Op == BindOp_Normal    && !createFileRaw_(PivotedBind, 0666)) ||
           (Op == BindOp_Directory && !makeDirectoryRaw_(PivotedBind, 0755)))
        {
            if(Options & Bind_Try)
            {
                // NOTE(nox): Error trying to create target, but it's OK
                return;
            }
            else
            {
                fprintf(stderr, "Could not create %s %s: %s\n", (Op == BindOp_Directory) ? "directory" : "file",
                        PivotedBind.Data, strerror(errno));
                exit(EXIT_FAILURE);
            }
        }

        if(mount((char *)PivotedBase.Data, (char *)PivotedBind.Data, 0, MS_SILENT|MS_BIND|MS_REC, 0) < 0)
        {
            if(Options & Bind_Try)
            {
                // NOTE(nox): Couldn't bind, but it's OK
                return;
            }
            else
            {
                fprintf(stderr, "Could not mount %s to %s: %s\n", PivotedBase.Data, PivotedBind.Data, strerror(errno));
                exit(EXIT_FAILURE);
            }
        }

        bindRemount(PivotedBind, Options);

        if(Op == BindOp_Directory)
        {
            // NOTE(nox): Need to remount recursively, in order to apply settings...

            while(PivotedBind.Data[PivotedBind.Size-1] == '/')
            {
                --PivotedBind.Size;
                PivotedBind.Data[PivotedBind.Size] = 0;
            }

            int MountInfo = openat(ProcFD, "self/mountinfo", O_RDONLY | O_CLOEXEC);
            if(MountInfo < 0)
            {
                fprintf(stderr, "Couldn't open /proc/self/mountinfo\n");
                exit(EXIT_FAILURE);
            }

            enum {
                MaxFileSize = mebiBytes(20),
                MaxRemountCount = 1000,
                RemountStringArraySize = MaxRemountCount*sizeof(string),
                RemountStringDataSize = MaxRemountCount*(MountPointMaximumLength+1),
                BlockSize = MaxFileSize + RemountStringArraySize + RemountStringDataSize,
            };
            u8 *BlockData = mmap(0, BlockSize, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_PRIVATE|MAP_NORESERVE, -1, 0);
            assert(BlockData);
            buffer Block = {
                .Size = BlockSize,
                .Data = BlockData,
            };

            buffer FileBuffer = {
                .Size = MaxFileSize-1,
                .Data = advance(&Block, MaxFileSize),
            };
            string FileStr = FileBuffer;

            for(;;) {
                ssize_t SizeRead;
                do {
                    SizeRead = read(MountInfo, FileBuffer.Data, FileBuffer.Size);
                } while(SizeRead < 0 && errno == EINTR);

                if(SizeRead < 0)
                {
                    fprintf(stderr, "Could not read mountinfo: %s\n", strerror(errno));
                    exit(EXIT_FAILURE);
                }
                else if(SizeRead == 0)
                {
                    break;
                }

                advance(&FileBuffer, SizeRead);
            }

            FileStr.Size = FileBuffer.Data - FileStr.Data;
            close(MountInfo);

            smm RemountCount = -1;
            string *ToRemount = (string *)advance(&Block, RemountStringArraySize);
            buffer RestorePoint = Block;

            for(;;)
            {
                // NOTE(nox): This assumes mountinfo stores everything in the order it was created!
                int BytesParsed;
                char EscapedMountPoint[MountPointMaximumLength+1];
                int ConversionCount = sscanf((char *)FileStr.Data,
                                             "%*s %*s %*s %*s %"stringify(MountPointMaximumLength)"s %*[^\n]\n%n",
                                             EscapedMountPoint, &BytesParsed);
                if(ConversionCount < 1)
                {
                    break;
                }

                advance(&FileStr, BytesParsed);
                string Escaped = wrapZ(EscapedMountPoint);

                char MountPointBuff[sizeof(EscapedMountPoint)];
                string MountPoint = { .Data = (u8 *)MountPointBuff };

                while(Escaped.Size)
                {
                    char Character = Escaped.Data[0];
                    advance(&Escaped, 1);

                    if(Character == '\\')
                    {
                        // NOTE(nox): Octal representation
                        assert(Escaped.Size >= 3);
                        Character = (((Escaped.Data[0] - '0') << 6) |
                                     ((Escaped.Data[1] - '0') << 3) |
                                     ((Escaped.Data[2] - '0') << 0));
                        advance(&Escaped, 3);
                    }

                    MountPoint.Data[MountPoint.Size++] = Character;
                }
                MountPoint.Data[MountPoint.Size] = 0;

                if(MountPoint.Size == PivotedBind.Size &&
                   memcmp(MountPoint.Data, PivotedBind.Data, PivotedBind.Size) == 0)
                {
                    RemountCount = 0;
                    Block = RestorePoint;
                }

                if(RemountCount >= 0 &&
                   MountPoint.Size > PivotedBind.Size &&
                   memcmp(MountPoint.Data, PivotedBind.Data, PivotedBind.Size) == 0)
                {
                    assert(RemountCount < MaxRemountCount);
                    ToRemount[RemountCount++] = formatString(&Block, "%s", MountPoint.Data);
                }
            }

            for(smm Idx = 0; Idx < RemountCount; ++Idx)
            {
                bindRemount(ToRemount[Idx], Options);
            }

            assert(munmap(BlockData, BlockSize) == 0);
        }
    }
}

internal void bindMount(char *BasePath, char *BindPath, bind_option Options)
{
    assert(BasePath && BasePath[0] == '/');
    assert(BindPath && BindPath[0] == '/');

    u8 Memory[1<<13] = {};
    buffer Buffer = bundleArray(Memory);

    string PivotedBase = formatString(&Buffer, BaseRootFolder"%s", BasePath);
    string PivotedBind = formatString(&Buffer, BindRootFolder"%s", BindPath);

    bindMountRaw(PivotedBase, PivotedBind, Options);
}

internal void bindMap(char *Prefix, char *Path, bind_option Options)
{
    assert(Path && Path[0] == '/');

    char *BasePath = Path;

    if(Prefix)
    {
        assert(Prefix[0] == '/');

        u8 Memory[1<<10] = {};
        string BaseStr = formatString(&bundleArray(Memory), "%s%s", Prefix, Path);

        BasePath = (char *)BaseStr.Data;
    }

    bindMount(BasePath, Path, Options);
}

internal void bindMapGlob(char *Prefix, char *Pattern, bind_option Options)
{
    assert(!Prefix ||  Prefix[0]  == '/');
    assert(Pattern && Pattern[0] == '/');

    u8 Memory[1<<14] = {};
    buffer Buffer = bundleArray(Memory);

    string PivotedPattern = (Prefix ?
                             formatString(&Buffer, BaseRootFolder"%s%s", Prefix, Pattern) :
                             formatString(&Buffer, BaseRootFolder"%s", Pattern));

    glob_t GlobResult = {};
    int ReturnValue = glob((char *)PivotedPattern.Data, GLOB_NOSORT, 0, &GlobResult);
    if(ReturnValue == 0)
    {
        for(umm Idx = 0; Idx < GlobResult.gl_pathc; ++Idx)
        {
            char *PivotedBasePath = GlobResult.gl_pathv[Idx];
            char *BasePath = PivotedBasePath + (sizeof(BaseRootFolder)-1);
            char *BindPath = BasePath + (Prefix ? strlen(Prefix) : 0);
            bindMount(BasePath, BindPath, Options);
        }
    }
    else if(ReturnValue == GLOB_NOMATCH && (Options & Bind_Try))
    {
        // NOTE(nox): Do nothing!
    }
    else
    {
        fprintf(stderr, "Error globing with pattern %s\n", Pattern);
        exit(EXIT_FAILURE);
    }
}

internal inline void bindAux(char *Path, b8 File)
{
    assert(Path && Path[0] == '/');

    u8 Memory[1<<10];
    string AuxPath = auxNode(&bundleArray(Memory), Path+1);

    bindMount((char *)AuxPath.Data, Path, File ? Bind_EnsureFile : Bind_EnsureDir);
}

typedef enum {
    Mount_Dev,
    Mount_DevPTS,
    Mount_Proc,
    Mount_Sys,
    Mount_Tmp,
} mount_type;

internal void otherMount(mount_type Type, char *BindPath)
{
    assert(BindPath && BindPath[0] == '/');

    u8 Memory[1<<13] = {};
    buffer Buffer = bundleArray(Memory);

    string PivotedBind = formatString(&Buffer, BindRootFolder"%s", BindPath);
    makeDirectoryRaw(PivotedBind, 0755);

    switch(Type) {
        case Mount_Dev: {
            otherMount(Mount_Tmp, BindPath);

            char *DevNodes[] = { "null", "zero", "full", "random", "urandom", "tty" };
            for(umm Idx = 0; Idx < arrayCount(DevNodes); ++Idx)
            {
                buffer TempBuffer = Buffer;

                string NodeSrc  = formatString(&TempBuffer, BaseRootFolder"/dev/%s",   DevNodes[Idx]);
                string NodeDest = formatString(&TempBuffer, "%s/%s", PivotedBind.Data, DevNodes[Idx]);

                bindMountRaw(NodeSrc, NodeDest, Bind_Dev);
            }

            char *StdIONodes[] = { "stdin", "stdout", "stderr" };
            for(umm Idx = 0; Idx < arrayCount(StdIONodes); ++Idx)
            {
                buffer TempBuffer = Buffer;

                string Target   = formatString(&TempBuffer, "/proc/self/fd/%d", Idx);
                string LinkPath = formatString(&TempBuffer, "%s/%s", PivotedBind.Data, StdIONodes[Idx]);
                symbolicLinkRaw(Target, LinkPath);
            }

            {
                buffer TempBuffer = Buffer;
                otherMount(Mount_DevPTS,       (char *)formatString(&TempBuffer, "%s/pts", BindPath).Data);
                symbolicLinkRaw(constZ("pts/ptmx"),    formatString(&TempBuffer, "%s/ptmx", PivotedBind.Data));
                symbolicLinkRaw(constZ("/proc/kcore"), formatString(&TempBuffer, "%s/core", PivotedBind.Data));
                symbolicLinkRaw(constZ("/proc/self/fd"), formatString(&TempBuffer, "%s/fd", PivotedBind.Data));
                makeDirectoryRaw(formatString(&TempBuffer, "%s/shm",    PivotedBind.Data), 01777);
                makeDirectoryRaw(formatString(&TempBuffer, "%s/mqueue", PivotedBind.Data), 01777);
            }

            if(BaseTTY)
            {
                buffer TempBuffer = Buffer;
                string BaseTTYDev  = formatString(&TempBuffer, BaseRootFolder"%s", BaseTTY);
                string BindConsole = formatString(&TempBuffer, "%s/console", PivotedBind.Data);

                bindMountRaw(BaseTTYDev, BindConsole, Bind_Dev);
            }
        } break;

        case Mount_DevPTS: {
            if(mount("devpts", (char *)PivotedBind.Data, "devpts", MS_SILENT|MS_NOATIME|MS_NOSUID|MS_NOEXEC, "ptmxmode=666") < 0)
            {
                fprintf(stderr, "Could not mount devpts on %s: %s\n", BindPath, strerror(errno));
                exit(EXIT_FAILURE);
            }
        } break;

        case Mount_Proc: {
            if(mount("proc", (char *)PivotedBind.Data, "proc", MS_SILENT|MS_NOSUID|MS_NOEXEC|MS_NODEV, 0) < 0)
            {
                fprintf(stderr, "Could not mount proc on %s: %s\n", BindPath, strerror(errno));
                exit(EXIT_FAILURE);
            }
        } break;

        case Mount_Sys: {
            bindMountRaw(constZ(BaseRootFolder"/sys"), PivotedBind, 0);
        } break;

        case Mount_Tmp: {
            if(mount("tmpfs", (char *)PivotedBind.Data, "tmpfs", MS_SILENT|MS_NOATIME|MS_NODEV, "mode=0777") < 0)
            {
                fprintf(stderr, "Could not mount tmpfs on %s: %s\n", BindPath, strerror(errno));
                exit(EXIT_FAILURE);
            }
        } break;
    }
}

internal void replaceFile_(char *FilePath, string Contents, mode_t ExtraMode)
{
    bindAux(FilePath + (sizeof(BindRootFolder)-1), true);

    int File = open(FilePath, O_WRONLY|O_CREAT|O_TRUNC|O_CLOEXEC, S_IRUSR|S_IWUSR|ExtraMode);
    if(File < 0)
    {
        fprintf(stderr, "Couldn't open %s: %s\n", FilePath, strerror(errno));
        exit(EXIT_FAILURE);
    }

    writeAll(File, Contents);
    close(File);
}
#define replaceFile(FilePath, Contents, ...) replaceFile_((BindRootFolder FilePath), (Contents), \
                                                          (sizeof(stringify(__VA_ARGS__)) > 1 ? __VA_ARGS__ : 0))

internal void cleanupEnvironmentVariables()
{
#define W(String) {.Size = sizeof(String)-1, .Data = (u8 *)(String)}
    string EnvsToKeep[] = { ENVS_TO_KEEP };
#undef W

    for(char **EnvPtr = environ; *EnvPtr;)
    {
        char *EnvEntry = EnvPtr[0];
        b8 KeepThis = false;

        for(umm TestIdx = 0; TestIdx < arrayCount(EnvsToKeep); ++TestIdx)
        {
            string Test = EnvsToKeep[TestIdx];
            char *TestStr = (char *)Test.Data;
            umm TestSize = Test.Size;

            if(strncmp(EnvEntry, TestStr, TestSize) == 0 && EnvEntry[TestSize] == '=')
            {
                KeepThis = true;
                break;
            }
        }

        if(!KeepThis)
        {
            char EnvName[1<<10];
            umm EnvSize = 0;

            for(;;)
            {
                if(EnvEntry[EnvSize] == 0 || EnvEntry[EnvSize] == '=')
                {
                    break;
                }

                assert(EnvSize < arrayCount(EnvName) - 1);
                EnvName[EnvSize] = EnvEntry[EnvSize];
                ++EnvSize;
            }
            EnvName[EnvSize] = 0;

            assert(unsetenv(EnvName) == 0);
        }
        else
        {
            ++EnvPtr;
        }
    }
}

typedef enum {
    Env_Set         = 0,
    Env_Append      = 1<<0,
    Env_Prepend     = 1<<1,
    Env_Colon       = 1<<2,

    Env_AppendColon = Env_Append  | Env_Colon,
    Env_PrependColon= Env_Prepend | Env_Colon,
} env_op;

internal void modifyEnvironmentVariable(char *EnvName, env_op Operation, char *Value)
{
    u8 Memory[1<<13];
    buffer Buffer = bundleArray(Memory);

    char *OldValue = getenv(EnvName);
    char *NewValue = Value;

    if(OldValue && Operation != Env_Set)
    {
        string NewValueString = formatString(&Buffer, "%s%s%s",
                                             Operation & Env_Append ? OldValue : NewValue,
                                             Operation & Env_Colon  ? ":" : "",
                                             Operation & Env_Append ? NewValue : OldValue);

        NewValue = (char *)NewValueString.Data;
    }

    if(setenv(EnvName, NewValue, true))
    {
        fprintf(stderr, "Could not set environment variable %s = %s: %s\n", EnvName, Value, strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal inline void setupBaseEnvironmentVariables()
{
    u8 Memory[1<<10];

    cleanupEnvironmentVariables();

    modifyEnvironmentVariable("USER",    Env_Set, getBindUserName());
    modifyEnvironmentVariable("LOGNAME", Env_Set, getBindUserName());

    modifyEnvironmentVariable("HOME", Env_Set, getBindHomePath(&bundleArray(Memory)));

    modifyEnvironmentVariable("PATH", Env_Set, BIND_ENV_DEFAULT_PATH);

    {
        string Value = formatString(&bundleArray(Memory), "/run/user/%lu", getBindUID());
        modifyEnvironmentVariable("XDG_RUNTIME_DIR", Env_Set, (char *)Value.Data);
    }

    char *LocaleEnvs[] = {
        "LC_ADDRESS", "LC_COLLATE", "LC_CTYPE", "LC_IDENTIFICATION", "LC_MONETARY", "LC_MESSAGES",
        "LC_MEASUREMENT", "LC_NAME", "LC_NUMERIC", "LC_PAPER", "LC_TELEPHONE", "LC_TIME", "LC_ALL"
    };
    for(umm Idx = 0; Idx < arrayCount(LocaleEnvs); ++Idx)
    {
        modifyEnvironmentVariable(LocaleEnvs[Idx], Env_Set, "C");
    }
    modifyEnvironmentVariable("TZ", Env_Set, "");
}

internal inline void setupDummyNetworkInterface()
{
#if defined(DUMMY_LINK)
    string DummyMAC = constZ(stringify(DUMMY_LINK)"");

    if((DummyMAC.Size == 1 && DummyMAC.Data[0] == '1') || DummyMAC.Size > 1)
    {
        if(DummyMAC.Size == 1)
        {
            DummyMAC = constZ("F8:16:54:E2:81:89");
        }

        {
            char *ShellArgs[] = {"ip", "link", "add", "eth0", "address", (char *)DummyMAC.Data, "type", "dummy", 0};
            runProgram(ShellArgs);
        }
        {
            char *ShellArgs[] = {"ip", "link", "set", "dev", "eth0", "up", 0};
            runProgram(ShellArgs);
        }
        {
            char *ShellArgs[] = {"ip", "address", "add", "192.168.1.2/24", "dev", "eth0", 0};
            runProgram(ShellArgs);
        }
    }
#endif
}

typedef enum {
    Rootfs_Minimal,
    Rootfs_Partial,
    Rootfs_Full,
    Rootfs_PassThrough,
} bind_rootfs_type;

internal void bindRootfs(char *Rootfs, bind_rootfs_type Type, bind_option ExtraOptions)
{
    switch(Type)
    {
        case Rootfs_Minimal:
        {
            bindMap(Rootfs, "/usr/bin",     ExtraOptions);
            bindMap(Rootfs, "/usr/include", ExtraOptions);
            bindMap(Rootfs, "/usr/lib",     ExtraOptions);
            bindMap(Rootfs, "/usr/lib32",   ExtraOptions | Bind_Try);
            bindMap(Rootfs, "/usr/lib64",   ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/usr/sbin",    ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/usr/share",   ExtraOptions);

            bindMap(Rootfs,     "/etc/OpenCL",          ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/X11",             ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/ca-certificates", ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/fonts",           ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/ld.so.cache",     ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/ld.so.conf",      ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/ld.so.conf.d",    ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/localtime",       ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/mime.types",      ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/nsswitch.conf",   ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/ssl",             ExtraOptions | Bind_Try);
            bindMap(Rootfs,     "/etc/xdg",             ExtraOptions | Bind_Try);
            bindMapGlob(Rootfs, "/etc/*-release",       ExtraOptions);

            bindMap(Rootfs, "/bin",   ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/sbin",  ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/lib",   ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/lib64", ExtraOptions | Bind_Try | Bind_KeepLinks);

            symbolicLink("/tmp", "/var/tmp");
            symbolicLink("/run", "/var/run");
        } break;

        case Rootfs_Partial:
        {
            bindMap(Rootfs, "/bin",   ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/etc",   ExtraOptions);
            bindMap(Rootfs, "/lib",   ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/lib64", ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/opt",   ExtraOptions | Bind_Try);
            bindMap(Rootfs, "/sbin",  ExtraOptions | Bind_Try | Bind_KeepLinks);
            bindMap(Rootfs, "/usr",   ExtraOptions);
            bindMap(Rootfs, "/var",   ExtraOptions);
        } break;

        case Rootfs_Full:
        case Rootfs_PassThrough:
        {
            FullBind = true;
            bindMap(Rootfs, "/", ExtraOptions);
        } break;
    }

    if(Type != Rootfs_PassThrough)
    {
        otherMount(Mount_Dev,  "/dev");
        otherMount(Mount_Proc, "/proc");
        otherMount(Mount_Tmp,  "/run");
        otherMount(Mount_Sys,  "/sys");
        otherMount(Mount_Tmp,  "/tmp");

        makeDirectory(getBindHomePath(&bundleArray((u8[1<<9]){})), 0700);
        makeDirectory(getenv("XDG_RUNTIME_DIR"), 0700);
        makeDirectory("/run/lock", 0755);
    }
}

internal inline void bindHome(char *Prefix, char *BasePath, bind_option ExtraOptions)
{
    assert(!Prefix   ||   Prefix[0] == '/');
    assert(!BasePath || BasePath[0] == '/');

    u8 Memory[1<<10];
    buffer Buffer = bundleArray(Memory);

    char *BindPath = getBindHomePath(&Buffer);

    string Base = formatString(&Buffer, "%s%s",
                               (Prefix ? Prefix : ""),
                               (BasePath ? BasePath : (!Prefix || Prefix == BaseUnionFS ? BaseHomePath : BindPath)));

    bindMount((char *)Base.Data, BindPath, ExtraOptions | Bind_EnsureDir);
    bindMount((char *)Base.Data, "/root",  ExtraOptions | Bind_EnsureDir);
}

internal inline void bindAuxHome(bind_option ExtraOptions)
{
    u8 Memory[1<<10];
    buffer Buffer = bundleArray(Memory);

    char *BindPath = getBindHomePath(&Buffer);
    string AuxPath = auxNode(&Buffer, BindPath+1);

    bindMount((char *)AuxPath.Data, BindPath, ExtraOptions | Bind_EnsureDir);
    bindMount((char *)AuxPath.Data, "/root",  ExtraOptions | Bind_EnsureDir);
}

internal inline void setupMachineID(char *FakeID)
{
    if(!FakeID)
    {
        // NOTE(nox): Map host machine ID
        bindMap(0, "/etc/machine-id", Bind_ReadOnly);
    }
    else
    {
        u8 Memory[1<<10];
        buffer Buffer = bundleArray(Memory);
        string Contents;

        string FakeIDString = wrapZ(FakeID);

        if(FakeIDString.Size == 32)
        {
            // NOTE(nox): Map fake machine ID
            Contents = formatString(&Buffer, "%s\n", FakeIDString.Data);
        }
        else if(FakeIDString.Size == 0)
        {
            // NOTE(nox): Generate random machine ID
            buffer UUIDBuffer = getSubBuffer(&Buffer, 16);
            for(umm Idx = 0; Idx < UUIDBuffer.Size; ++Idx)
            {
                UUIDBuffer.Data[Idx] = rand() & ((1<<8) - 1);
            }

            u64 *UUID = consumeArray(&UUIDBuffer, u64, 2);
            Contents = formatString(&Buffer, "%016lx%016lx\n", UUID[0],  UUID[1]);
        }
        else
        {
            fprintf(stderr, "Invalid machine-id string: %s\n", FakeID);
            exit(EXIT_FAILURE);
        }

        replaceFile("/etc/machine-id", Contents);
    }
}

internal inline void shareDisplay()
{
    string BindXAuth = constZ("/tmp/xauth");

    modifyEnvironmentVariable("XAUTHORITY", Env_Set, (char *)BindXAuth.Data);
    bindMount(BaseXAuth, (char *)BindXAuth.Data, Bind_ReadOnly);

    bindMap(0, "/tmp/.X11-unix/X0", Bind_ReadOnly);
}

internal inline void shareGraphics()
{
    bindMap(0, "/dev/dri", Bind_Dev);

    bindMap(0, "/dev/nvidia0",        Bind_Dev | Bind_Try);
    bindMap(0, "/dev/nvidiactl",      Bind_Dev | Bind_Try);
    bindMap(0, "/dev/nvidia-modeset", Bind_Dev | Bind_Try);
    bindMount("/usr/bin/true", "/usr/bin/nvidia-modprobe", Bind_ReadOnly | Bind_Try);
}

internal inline void shareAudio(b32 BindConfig)
{
    if(BindConfig)
    {
        bindMap(0, "/etc/alsa", Bind_ReadOnly);
        bindMap(0, "/etc/asound.conf", Bind_ReadOnly | Bind_Try);
        bindMap(0, "/etc/pulse", Bind_ReadOnly);
    }

    u8 Memory[1<<12];
    bindMap(0, "/dev/snd", Bind_Dev);
    {
        buffer Buffer = bundleArray(Memory);
        string Base = formatString(&Buffer, "/run/user/%lu/pulse", BaseUID);
        string Bind = formatString(&Buffer, "/run/user/%lu/pulse", getBindUID());
        bindMount((char *)Base.Data, (char *)Bind.Data, Bind_ReadOnly);

        string PulseServer = formatString(&Buffer, "unix:%s/native", Bind.Data);
        modifyEnvironmentVariable("PULSE_SERVER", Env_Set, (char *)PulseServer.Data);
    }
}

internal inline void shareInput()
{
    bindMap(0, "/dev/input",  Bind_Dev);
    bindMap(0, "/dev/uinput", Bind_Dev);
}

internal inline void setupNetwork()
{
    u8 Memory[1<<13];
    buffer Buffer = bundleArray(Memory);

    string Hostname = constZ(stringify(BIND_HOSTNAME)"");
    if(Hostname.Size)
    {
        if(sethostname((char *)Hostname.Data, Hostname.Size) < 0)
        {
            fprintf(stderr, "Could not set host name to %s\n", Hostname.Data);
            exit(EXIT_FAILURE);
        }
    }

#if SHARE_NETWORK
    puts(colorWarn("===   Sharing network!   ==="));
    bindMap(0, "/etc/resolv.conf", Bind_ReadOnly | Bind_Try);
    bindMap(0, "/etc/hosts",       Bind_ReadOnly | Bind_Try);
#else
    {
        int Socket = socket(AF_INET, SOCK_DGRAM|SOCK_CLOEXEC, 0);
        if(Socket < 0)
        {
            fprintf(stderr, "Could not open network management socket: %s\n", strerror(errno));
            exit(EXIT_FAILURE);
        }

        struct ifreq InterfaceSetup = {
            .ifr_name = "lo",
            .ifr_flags = IFF_UP,
        };
        if(ioctl(Socket, SIOCSIFFLAGS, &InterfaceSetup) < 0)
        {
            fprintf(stderr, "Could not configure interface: %s\n", strerror(errno));
            exit(EXIT_FAILURE);
        }
        close(Socket);
    }

    if(Hostname.Size == 0)
    {
        bindMap(0, "/etc/hosts", Bind_ReadOnly | Bind_Try);
    }
    else
    {
        replaceFile("/etc/hosts", formatString(&Buffer, "127.0.0.1 localhost %s %s.localdomain\n",
                                               Hostname.Data, Hostname.Data));
    }
#endif
}

internal void setupDBusParent()
{
#if SHARE_DBUS
    u8 Memory[1<<13];
    buffer Buffer = bundleArray(Memory);

    string SystDBusPath  = constZ("/run/dbus/system_bus_socket");
    string SystProxyAddr = formatString(&Buffer, "unix:path=%s", SystDBusPath.Data);
    string SystProxyPath = auxNode(&Buffer, "systdbus");

    string UserDBusPath  = formatString(&Buffer, "/run/user/%lu/bus", BaseUID);
    string UserProxyAddr = formatString(&Buffer, "unix:path=%s", UserDBusPath.Data);
    string UserProxyPath = auxNode(&Buffer, "userdbus");

    char *ProxyShellArgs[] = {
        "xdg-dbus-proxy",

        (char *)SystProxyAddr.Data,
        (char *)SystProxyPath.Data,
        DBUS_SYST_PROXY

        (char *)UserProxyAddr.Data,
        (char *)UserProxyPath.Data,
        DBUS_USER_PROXY

        0
    };

    // NOTE(nox): Use proxy for system DBus
    runProgramInBackground(ProxyShellArgs);

    BaseSystDBus = strdup((char *)SystProxyPath.Data);
    BaseUserDBus = strdup((char *)UserProxyPath.Data);
#endif
}

internal void setupDBus()
{
#if SHARE_DBUS
    u8 Memory[1<<13];
    buffer Buffer = bundleArray(Memory);
    string SystDBusPath  = constZ("/run/dbus/system_bus_socket");
    string UserDBusPath  = formatString(&Buffer, "/run/user/%lu/bus", getBindUID());

    bindMount(BaseSystDBus, (char *)SystDBusPath.Data, Bind_ReadOnly);
    bindMount(BaseUserDBus, (char *)UserDBusPath.Data, Bind_ReadOnly);

    string DBusSessionAddr = formatString(&Buffer, "unix:path=%s", UserDBusPath.Data);
    modifyEnvironmentVariable("DBUS_SESSION_BUS_ADDRESS", Env_Set, (char *)DBusSessionAddr.Data);

    modifyEnvironmentVariable("DBUS_FATAL_WARNINGS", Env_Set, "0");
#endif
}

internal void bindCustomWineBuild(char *BasePath)
{
#define WineBindPath    "/wine"
#define WineBindBinPath WineBindPath"/bin"

    bindMount(BasePath, WineBindPath, Bind_ReadOnly);
    modifyEnvironmentVariable("PATH",            Env_PrependColon, WineBindBinPath);
    modifyEnvironmentVariable("LD_LIBRARY_PATH", Env_PrependColon, WineBindPath"/lib64:"WineBindPath"/lib:"WineBindPath"/lib32");

#undef WineBindPath
#undef WineBindBinPath
}

internal inline void setupUsersAndGroups()
{
    u8 Memory[1<<13];
    buffer Buffer = bundleArray(Memory);

    char *BindHomePath = getBindHomePath(&Buffer);

    string Passwd = formatString(&Buffer,
                                 "%s:x:%lu:%lu:%s:%s:/usr/bin/bash\n"
                                 "root:x:%lu:%lu:::/sbin/nologin\n"
                                 "nobody:x:%lu:%lu:::/sbin/nologin\n"
                                 "postfix:x:%lu:%lu:::/sbin/nologin\n",
                                 getBindUserName(), getBindUID(), getBindGID(), getBindUserName(), BindHomePath,
                                 getBindUID(), getBindGID(),
                                 getBindUID(), getBindGID(),
                                 getBindUID(), getBindGID());
    replaceFile("/etc/passwd", Passwd);

    replaceFile("/etc/group", formatString(&Buffer,
                                           "%s:x:%lu:%s\n"
                                           "root:x:%lu:\n"
                                           "nobody:x:%lu:\n"
                                           "audio:x:%lu:\n"
                                           "tty:x:%lu:\n"
                                           "postdrop:x:%lu:\n"
                                           "postfix:x:%lu:\n"
                                           "mail:x:%lu:\n",
                                           getBindUserName(), getBindGID(), getBindUserName(),
                                           getBindGID(), getBindGID(), getBindGID(),
                                           getBindGID(), getBindGID(), getBindGID(),
                                           getBindGID()));
}

internal inline void setupProcVersion()
{
#if defined(PROC_VERSION)
    if(constZ(PROC_VERSION"").Size)
    {
        replaceFile("/proc/version", constZ(PROC_VERSION"\n"));
    }
#endif
}

internal void setupSeccomp()
{
    struct sock_filter Filter[] = {
        BPF_STMT(BPF_LD  | BPF_W   | BPF_ABS, (offsetof(struct seccomp_data, arch))),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,   AUDIT_ARCH_X86_64, 0, 2),
        BPF_STMT(BPF_LD  | BPF_W   | BPF_ABS, (offsetof(struct seccomp_data, nr))),
        BPF_JUMP(BPF_JMP | BPF_JGE | BPF_K,   __X32_SYSCALL_BIT, 0, 1),
        BPF_STMT(BPF_RET | BPF_K,             SECCOMP_RET_ALLOW),

        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,   SYS_chown,    4, 0),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,   SYS_fchown,   3, 0),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,   SYS_fchownat, 2, 0),
        BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K,   SYS_lchown,   1, 0),

        BPF_STMT(BPF_RET | BPF_K,             SECCOMP_RET_ALLOW),
        BPF_STMT(BPF_RET | BPF_K,             SECCOMP_RET_ERRNO),
    };

    struct sock_fprog SeccompProgram = {
        .len = arrayCount(Filter),
        .filter = Filter,
    };

    if(prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &SeccompProgram) < 0)
    {
        fprintf(stderr, "Couldn't set seccomp program: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal inline void setupBaseAndBindRoots()
{
    if(mount("tmpfs", "/tmp", "tmpfs", MS_SILENT|MS_NOATIME|MS_NODEV, "mode=0755,size=100g") < 0)
    {
        fprintf(stderr, "Could not mount tmpfs\n");
        exit(EXIT_FAILURE);
    }

    if(mkdir("/tmp"BindRootFolder, 0755) < 0 || mkdir("/tmp"BaseRootFolder, 0755) < 0)
    {
        fprintf(stderr, "Could not create folders for holding the roots\n");
        exit(EXIT_FAILURE);
    }

    bindMountRaw(constZ("/tmp"BindRootFolder), constZ("/tmp"BindRootFolder), 0);

    if(pivotRootSC("/tmp", "/tmp"BaseRootFolder) < 0 || chdir("/") < 0)
    {
        fprintf(stderr, "Could not pivot roots\n");
        exit(EXIT_FAILURE);
    }

    // NOTE(nox): These helper binds serve as a reference for symbolic link targets
    for(umm Idx = 0; Idx < arrayCount(HelperBinds); ++Idx)
    {
        string HelperBind = HelperBinds[Idx];

        u8 Memory[1<<10] = {};
        string PivotedBase = formatString(&bundleArray(Memory), BaseRootFolder"%s", HelperBind.Char);

        bindMountRaw(PivotedBase, HelperBind, 0);
    }
}

internal inline void switchToBindRoot()
{
    if(umount2(BaseRootFolder, MNT_DETACH) < 0)
    {
        fprintf(stderr, "Could not unmount base root: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    if(chdir("/") < 0)
    {
        fprintf(stderr, "Could not change directory to temporary root: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    if(pivotRootSC(BindRootFolder, BindRootFolder) < 0)
    {
        fprintf(stderr, "Could not chroot to bind root\n");
        exit(EXIT_FAILURE);
    }

    if(umount2(".", MNT_DETACH) < 0)
    {
        fprintf(stderr, "Could not unmount temporary root: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    if(chdir("/") < 0)
    {
        fprintf(stderr, "Could not change directory to bind root: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal inline void changeToUsefulDirectory()
{
    u8 Memory[1<<10];
    (void)(chdir(InitialWorkingDirectory)               == 0 ||
           chdir(getBindHomePath(&bundleArray(Memory))) == 0);
}

internal inline b8 shouldOverrideArgs(int ArgCount, char *ArgVals[])
{
    b8 Result = (ArgCount > 1 && strcmp(ArgVals[0], ARGS_OVERRIDE_STRING) == 0);

    return Result;
}

internal inline void execute_(int ArgCount, char *ArgVals[], u32 ShellCount, char *ShellArguments[])
{
    char **FinalShellArgs = 0;

    if(ArgCount == 0)
    {
        // NOTE(nox): No additional arguments
        FinalShellArgs = calloc(ShellCount + 1, sizeof(char *));
        memcpy(FinalShellArgs, ShellArguments, ShellCount*sizeof(char *));
    }
    else
    {
        if(shouldOverrideArgs(ArgCount, ArgVals))
        {
            // NOTE(nox): Complete override
            FinalShellArgs = calloc(ArgCount, sizeof(char *));
            memcpy(FinalShellArgs, ArgVals+1, (ArgCount-1)*sizeof(char *));
        }
        else
        {
            // NOTE(nox): Additional arguments
            FinalShellArgs = calloc(ShellCount + ArgCount + 1, sizeof(char *));
            memcpy(FinalShellArgs + 0,          ShellArguments, ShellCount*sizeof(char *));
            memcpy(FinalShellArgs + ShellCount, ArgVals,          ArgCount*sizeof(char *));
        }
    }

    if(execvp(FinalShellArgs[0], FinalShellArgs) < 0)
    {
        fprintf(stderr, "Couldn't execute '%s'\n", FinalShellArgs[0]);
        exit(EXIT_FAILURE);
    }
}
#define execute(ArgCount, ArgVals, ShellArguments) execute_(ArgCount, ArgVals, arrayCount(ShellArguments), ShellArguments)

CONFIGURE_CONTAINER();
RUN_COMMAND();

int main(int ArgCount, char *ArgVals[])
{
    --ArgCount;
    ++ArgVals;

    srand(time(0));

    ParentPID = getpid();
    InitialWorkingDirectory = get_current_dir_name();

    createAuxDirectory();

    BaseUID = geteuid();
    BaseGID = getegid();
    assert(getlogin_r(BaseUserName, sizeof(BaseUserName)) == 0);

    BaseXAuth = strdup(getenv("XAUTHORITY"));
    BaseHomePath = strdup(getenv("HOME"));

    createUnionFS();
    setupDBusParent();

    sigset_t SigMask;
    sigemptyset(&SigMask); sigaddset(&SigMask, SIGUSR1);
    sigprocmask(SIG_BLOCK, &SigMask, 0);

    u64 Flags = (SIGCHLD | CLONE_NEWUSER | CLONE_NEWPID | CLONE_NEWNS | CLONE_NEWUTS |
                 (SHARE_NETWORK ? 0 : CLONE_NEWNET));
    pid_t ChildPID = cloneSC(Flags);

    if(ChildPID > 0)
    {
        // NOTE(nox): Parent
        sigprocmask(SIG_UNBLOCK, &SigMask, 0);

        if(isatty(STDIN_FILENO))
        {
            assert(setpgid(ChildPID, ChildPID) == 0);
            assert(tcsetpgrp(STDIN_FILENO, ChildPID) == 0);
        }

        disableSetGroups(ChildPID);

        mapID(ChildPID, Map_UID, BaseUID, getBindUID());
        mapID(ChildPID, Map_GID, BaseGID, getBindGID());

        kill(ChildPID, SIGUSR1);
        waitpid(ChildPID, 0, 0);
    }
    else if(ChildPID == 0)
    {
        // NOTE(nox): Child
        dieWithParent();

        umask(0);

        if(isatty(STDIN_FILENO))
        {
            BaseTTY = ttyname(STDIN_FILENO);
        }

        ProcFD = open("/proc", O_PATH | O_DIRECTORY | O_CLOEXEC);
        if(ProcFD < 0)
        {
            fprintf(stderr, "Could not open /proc\n");
            exit(EXIT_FAILURE);
        }

        while(sigwaitinfo(&SigMask, 0) < 0);
        sigprocmask(SIG_UNBLOCK, &SigMask, 0);

        keepCaps();
        setupBaseEnvironmentVariables();
        setupDummyNetworkInterface();

        setupBaseAndBindRoots();
        {
            configureContainer();
            setupUsersAndGroups();
            setupProcVersion();
            setupNetwork();
            setupSeccomp();
            setupDBus();
        }
        switchToBindRoot();

        if(!FullBind)
        {
            // NOTE(nox): Prevent creating files & folders in the root tmpfs, which is gone as soon as the
            // container stops
            bindRemount(constZ("/"), Bind_ReadOnly);
        }

        dropCaps();
        umask(0022);
        changeToUsefulDirectory();
        runCommand(ArgCount, ArgVals);

        // NOTE(nox): This should never run!
        abort();
    }
    else
    {
        // NOTE(nox): Error while cloning
        fprintf(stderr, "Could not clone: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    unmountUnionFS();
#if !defined(KEEP_AUX) || !KEEP_AUX
    deletePathRaw(AuxDirectory);
#endif

    return 0;
}
// Local Variables:
// mode: c
// End:
