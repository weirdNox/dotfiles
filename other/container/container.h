#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <net/if.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
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

#define arrayCount(Arr) (sizeof(Arr)/sizeof(*(Arr)))

#define kibiBytes(X) (         (X) * 1024LL)
#define mebiBytes(X) (kibiBytes(X) * 1024LL)
#define gibiBytes(X) (mebiBytes(X) * 1024LL)

#define staticAssert(Expr) static_assert(Expr, "Assertion failed: "#Expr)

#define internal static
#define global_variable static


#define BaseRootFolder "/base"
#define BindRootFolder "/bind"

#define MountPointMaximumLength 255

#define CONFIGURE_CONTAINER() internal void configureContainer()
#define RUN_COMMAND() internal void runCommand()
    ;

global_variable char *BaseTTY;
global_variable int ProcFD;


typedef struct {
    umm Size;
    u8 *Data;
} buffer;

typedef buffer string;

#define bundleArray(Array) (buffer){ .Size = sizeof(Array), .Data = (u8 *)(Array) }
#define constZ(String) (string){ .Size = sizeof(String)-1, .Data = (u8 *)(String) }

internal inline string wrapZ(char *String)
{
    string Result = {
        .Size = strlen(String),
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

    return(Result);
}

internal inline string formatString(buffer *Buffer, char *Format, ...)
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
    Buffer->Size -= UsedBytes;
    Buffer->Data += UsedBytes;

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

internal void makeDirectory(string Path, mode_t Mode)
{
    u8 Buffer[1<<13];

    if(Path.Size + 1 <= sizeof(Buffer))
    {
        memcpy(Buffer, Path.Data, Path.Size);
        Buffer[Path.Size] = 0;

        u8 *Iter = Buffer;

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

            struct stat Stat;
            if(stat((char *)Buffer, &Stat) == 0)
            {
                // NOTE(nox): Node exists and is accessible
                if(!S_ISDIR(Stat.st_mode))
                {
                    fprintf(stderr, "Node %s already exists and isn't a directory\n", Buffer);
                    exit(EXIT_FAILURE);
                }
            }
            else
            {
                if(mkdir((char *)Buffer, Mode) == -1 && errno != EEXIST)
                {
                    fprintf(stderr, "Failed to create directory %s\n", Buffer);
                    exit(EXIT_FAILURE);
                }
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
}

internal void createFile(string Path, mode_t Mode)
{
    int File = creat((char *)Path.Data, Mode);
    if(File < 0)
    {
        fprintf(stderr, "Could not create file %s: %s\n", Path.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }
}

internal void symbolicLink(string Target, string LinkPath)
{
    if(symlink((char *)Target.Data, (char *)LinkPath.Data) < 0)
    {
        fprintf(stderr, "Could not create symlink %s (-> %s): %s\n", LinkPath.Data, Target.Data, strerror(errno));
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

internal void disableSetGroups(pid_t ChildPID)
{
    char FileName[100];
    formatString(&bundleArray(FileName), "/proc/%d/setgroups", ChildPID);

    int File = open(FileName, O_WRONLY);
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

    int File = open(FileName, O_WRONLY);
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

internal inline void dieWithParent()
{
    if(prctl(PR_SET_PDEATHSIG, SIGKILL) < 0)
    {
        fprintf(stderr, "Could not set parent death signal\n");
        exit(EXIT_FAILURE);
    }
}

typedef enum {
    Bind_Dev      = 1<<1,
    Bind_ReadOnly = 1<<2,
    Bind_Try      = 1<<3,
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
    struct stat Stat;
    if(stat((char *)PivotedBase.Data, &Stat) < 0)
    {
        if(Options & Bind_Try)
        {
            // NOTE(nox): Trying to bind an inexistent file, but it's OK
            return;
        }
        else {
            fprintf(stderr, "Could not stat required file %s: %s\n", PivotedBase.Data, strerror(errno));
            exit(EXIT_FAILURE);
        }
    }

    b8 BindingDirectory = S_ISDIR(Stat.st_mode);

    if(BindingDirectory)
    {
        makeDirectory(PivotedBind, 0755);
    }
    else {
        createFile(PivotedBind, 0666);
    }

    if(mount((char *)PivotedBase.Data, (char *)PivotedBind.Data, 0, MS_SILENT|MS_BIND|MS_REC, 0) < 0)
    {
        fprintf(stderr, "Could not mount %s to %s: %s\n", PivotedBase.Data, PivotedBind.Data, strerror(errno));
        exit(EXIT_FAILURE);
    }

    bindRemount(PivotedBind, Options);

    if(BindingDirectory)
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

        umm RemountCount = (umm)-1;
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

            if(RemountCount != (umm)-1 &&
               MountPoint.Size > PivotedBind.Size &&
               memcmp(MountPoint.Data, PivotedBind.Data, PivotedBind.Size) == 0)
            {
                assert(RemountCount < MaxRemountCount);
                ToRemount[RemountCount++] = formatString(&Block, "%s", MountPoint.Data);
            }
        }

        for(umm Idx = 0; Idx < RemountCount; ++Idx)
        {
            bindRemount(ToRemount[Idx], Options);
        }

        assert(munmap(BlockData, BlockSize) == 0);
    }
}

internal void bindMount(char *BasePath, char *BindPath, bind_option Options)
{
    assert(BasePath[0] == '/' && BindPath[0] == '/');

    u8 Memory[1<<13] = {};
    buffer Buffer = bundleArray(Memory);

    string PivotedBase = formatString(&Buffer, BaseRootFolder"%s", BasePath);
    string PivotedBind = formatString(&Buffer, BindRootFolder"%s", BindPath);

    bindMountRaw(PivotedBase, PivotedBind, Options);
}

typedef enum {
    Mount_Dev,
    Mount_DevPTS,
    Mount_Proc,
    Mount_Tmp,
} mount_type;

internal void otherMount(mount_type Type, char *BindPath)
{
    assert(BindPath[0] == '/');

    u8 Memory[1<<13] = {};
    buffer Buffer = bundleArray(Memory);

    string PivotedBind = formatString(&Buffer, BindRootFolder"%s", BindPath);
    makeDirectory(PivotedBind, 0755);

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
                symbolicLink(Target, LinkPath);
            }

            {
                buffer TempBuffer = Buffer;

                string FDLink   = formatString(&TempBuffer, "%s/fd",   PivotedBind.Data);
                string CoreLink = formatString(&TempBuffer, "%s/core", PivotedBind.Data);
                symbolicLink(constZ("/proc/self/fd"),    FDLink);
                symbolicLink(constZ("/proc/self/kcore"), CoreLink);
            }

            {
                buffer TempBuffer = Buffer;

                string SHM = formatString(&TempBuffer, "%s/shm", PivotedBind.Data);
                makeDirectory(SHM, 0755);

                string PTS = formatString(&TempBuffer, "%s/pts", BindPath);
                otherMount(Mount_DevPTS, (char *)PTS.Data);

                string PTMX = formatString(&TempBuffer, "%s/ptmx", PivotedBind.Data);
                symbolicLink(constZ("pts/ptmx"), PTMX);
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
            if(mount("devpts", (char *)PivotedBind.Data, "devpts", MS_SILENT|MS_NOATIME|MS_NOSUID|MS_NOEXEC, 0) < 0)
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

        case Mount_Tmp: {
            if(mount("tmpfs", (char *)PivotedBind.Data, "tmpfs", MS_SILENT|MS_NOATIME|MS_NODEV, "mode=0755") < 0)
            {
                fprintf(stderr, "Could not mount sys on %s: %s\n", BindPath, strerror(errno));
                exit(EXIT_FAILURE);
            }
        } break;
    }
}

internal inline void setupBaseEnvironmentVariables()
{
    assert(clearenv() == 0);

    assert(setenv("USER",    BindUser, true) == 0);
    assert(setenv("LOGNAME", BindUser, true) == 0);

    assert(setenv("HOME", BindHome, true) == 0);

}

internal inline void setupNetwork()
{
#if UnshareNet
    struct ifreq InterfaceSetup = {
        .ifr_name = "lo",
        .ifr_flags = IFF_UP,
    };

    int Socket = socket(AF_INET, SOCK_DGRAM, 0);
    if(Socket < 0)
    {
        fprintf(stderr, "Could not open network management socket: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    if(ioctl(Socket, SIOCSIFFLAGS, &InterfaceSetup) < 0)
    {
        fprintf(stderr, "Could not configure interface: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }
#endif

    string HostStr = constZ(HostName);
    if(sethostname((char *)HostStr.Data, HostStr.Size) < 0)
    {
        fprintf(stderr, "Could not set host name to "HostName"\n");
        exit(EXIT_FAILURE);
    }
}

internal inline void setupBaseAndBindRoots()
{
    if(mount("tmpfs", "/tmp", "tmpfs", MS_SILENT|MS_NOATIME|MS_NODEV, 0) < 0)
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

internal inline void execute(char *ShellArguments[])
{
    if(execvp(ShellArguments[0], ShellArguments) < 0)
    {
        fprintf(stderr, "Couldn't execute '%s'\n", ShellArguments[0]);
        exit(EXIT_FAILURE);
    }
}

CONFIGURE_CONTAINER();
RUN_COMMAND();

int main()
{
    sigset_t SigMask;
    sigemptyset(&SigMask); sigaddset(&SigMask, SIGUSR1);
    sigprocmask(SIG_BLOCK, &SigMask, 0);

    u64 Flags = (SIGCHLD | CLONE_NEWUSER | CLONE_NEWPID | CLONE_NEWNS | CLONE_NEWUTS |
                 (UnshareNet ? CLONE_NEWNET : 0));
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

        uid_t BaseUID = geteuid();
        gid_t BaseGID = getegid();
        mapID(ChildPID, Map_UID, BaseUID, BindUID < 0 ? geteuid() : BindUID);
        mapID(ChildPID, Map_GID, BaseGID, BindGID < 0 ? getegid() : BindGID);

        kill(ChildPID, SIGUSR1);
        waitpid(ChildPID, 0, 0);
    }
    else if(ChildPID == 0)
    {
        // NOTE(nox): Child
        dieWithParent();

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
        setupNetwork();

        setupBaseAndBindRoots();
        configureContainer();
        switchToBindRoot();
        runCommand();

        // NOTE(nox): This should never run!
        abort();
    }
    else
    {
        // NOTE(nox): Error while cloning
        fprintf(stderr, "Could not clone: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    return 0;
}

// Local Variables:
// mode: c
// End:
