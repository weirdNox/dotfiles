#!/usr/bin/tcc -run

#define _GNU_SOURCE
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <sched.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/capability.h>
#include <sys/mount.h>
#include <sys/prctl.h>
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

#define arrayCount(Arr) (sizeof(Arr)/sizeof(*(Arr)))

#define kibiBytes(X) (         (X) * 1024LL)
#define mebiBytes(X) (kibiBytes(X) * 1024LL)
#define gibiBytes(X) (mebiBytes(X) * 1024LL)

#define staticAssert(Expr) static_assert(Expr, "Assertion failed: "#Expr)

#define internal static
#define global_variable static


#define BaseRootFolder "/base"
#define BindRootFolder "/bind"

global_variable char *BaseTTY;


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
        Buffer->Data += Buffer->Size;
        Buffer->Size  = 0;
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
            printf("Error writing to file: %s\n", strerror(errno));
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
} bind_option;

internal inline u64 getExtraMountFlags(bind_option Options)
{
    u64 Result = (((Options & Bind_Dev)      ? 0         : MS_NODEV) |
                  ((Options & Bind_ReadOnly) ? MS_RDONLY : 0       ));

    return Result;
}

internal inline int bindMount_(string PivotedBase, string PivotedBind, bind_option Options)
{
    int Result;

    if((Result = mount((char *)PivotedBase.Data, (char *)PivotedBind.Data, 0, MS_SILENT|MS_BIND|MS_REC, 0)) == 0)
    {
        struct statvfs FileSystemInfo;
        statvfs((char *)PivotedBind.Data, &FileSystemInfo);
        u64 CurrentFlags = FileSystemInfo.f_flag;

        u64 NewFlags = MS_SILENT | MS_BIND | MS_REMOUNT | getExtraMountFlags(Options);

        Result = mount(0, (char *)PivotedBind.Data, 0, CurrentFlags | NewFlags, 0);
    }

    return Result;
}

internal void bindMount(char *BasePath, char *BindPath, bind_option Options)
{
    assert(BasePath[0] == '/' && BindPath[0] == '/');

    u8 Memory[1<<13] = {};
    buffer Buffer = bundleArray(Memory);

    string PivotedBase = formatString(&Buffer, BaseRootFolder"%s", BasePath);
    string PivotedBind = formatString(&Buffer, BindRootFolder"%s", BindPath);

    struct stat Stat;
    if(stat((char *)PivotedBase.Data, &Stat) != 0)
    {
        fprintf(stderr, "Could not stat %s: %s\n", BasePath, strerror(errno));
        exit(EXIT_FAILURE);
    }

    if(S_ISDIR(Stat.st_mode))
    {
        makeDirectory(PivotedBind, 0755);
    }
    else {
        createFile(PivotedBind, 0666);
    }

    if(bindMount_(PivotedBase, PivotedBind, Options) < 0)
    {
        fprintf(stderr, "Could not mount %s to %s: %s\n", BasePath, BindPath, strerror(errno));
        exit(EXIT_FAILURE);
    }
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

                if(createFile(NodeDest, 0666), bindMount_(NodeSrc, NodeDest, Bind_Dev) < 0)
                {
                    fprintf(stderr, "Could not mount /dev/%s to %s/%s: %s\n", DevNodes[Idx], BindPath, DevNodes[Idx], strerror(errno));
                    exit(EXIT_FAILURE);
                }
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

                if(createFile(BindConsole, 0666), bindMount_(BaseTTYDev, BindConsole, Bind_Dev) < 0)
                {
                    fprintf(stderr, "Could not mount %s to %s/console: %s\n", BaseTTY, BindPath, strerror(errno));
                    exit(EXIT_FAILURE);
                }
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

        case Mount_Sys: {
            if(mount("sysfs", (char *)PivotedBind.Data, "sysfs", MS_SILENT|MS_NOSUID|MS_NOEXEC|MS_NODEV, 0) < 0)
            {
                fprintf(stderr, "Could not mount sys on %s: %s\n", BindPath, strerror(errno));
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

internal inline void setupBaseAndBindRoots()
{
    if(mount("tmpfs", "/tmp", "tmpfs", MS_SILENT|MS_NOATIME|MS_NODEV, 0) < 0)
    {
        fprintf(stderr, "Could not mount tmpfs\n");
        exit(EXIT_FAILURE);
    }
    else {
        if(mkdir("/tmp"BindRootFolder, 0755) < 0 || mkdir("/tmp"BaseRootFolder, 0755) < 0)
        {
            fprintf(stderr, "Could not create folders for holding the roots\n");
            exit(EXIT_FAILURE);
        }
        else {
            if(bindMount_(constZ("/tmp"BindRootFolder), constZ("/tmp"BindRootFolder), 0) < 0)
            {
                fprintf(stderr, "Could not create root bind\n");
                exit(EXIT_FAILURE);
            }
            else {
                if(pivotRootSC("/tmp", "/tmp"BaseRootFolder) < 0 || chdir("/") < 0)
                {
                    fprintf(stderr, "Could not pivot roots\n");
                    exit(EXIT_FAILURE);
                }
            }
        }
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

int main()
{
    uid_t BaseUID = geteuid();
    gid_t BaseGID = getegid();

    if(isatty(STDOUT_FILENO))
    {
        BaseTTY = ttyname(STDOUT_FILENO);
    }

    sigset_t SigMask;
    sigemptyset(&SigMask); sigaddset(&SigMask, SIGUSR1);
    sigprocmask(SIG_BLOCK, &SigMask, 0);

    u64 Flags = CLONE_NEWUSER | CLONE_NEWNET | CLONE_NEWPID | CLONE_NEWNS | CLONE_NEWUTS | SIGCHLD;
    pid_t ChildPID = cloneSC(Flags);

    if(ChildPID > 0)
    {
        // NOTE(nox): Parent
        sigprocmask(SIG_UNBLOCK, &SigMask, 0);

        assert(setpgid(ChildPID, ChildPID) == 0);
        assert(tcsetpgrp(STDIN_FILENO, ChildPID) == 0);

        disableSetGroups(ChildPID);

        // TODO(nox): Allow customization
        mapID(ChildPID, Map_UID, BaseUID, 1000);
        mapID(ChildPID, Map_GID, BaseGID, 1000);

        kill(ChildPID, SIGUSR1);
        waitpid(ChildPID, 0, 0);
    }
    else if(ChildPID == 0)
    {
        // NOTE(nox): Child
        while(sigwaitinfo(&SigMask, 0) < 0);
        sigprocmask(SIG_UNBLOCK, &SigMask, 0);

        dieWithParent();
        keepCaps();

        setupBaseAndBindRoots();

        otherMount(Mount_Dev,  "/dev");
        otherMount(Mount_Proc, "/proc");
        otherMount(Mount_Sys,  "/sys");

        bindMount("/etc",   "/etc",     0);
        bindMount("/lib",   "/lib",     0);
        bindMount("/lib64", "/lib64",   0);
        bindMount("/usr",   "/usr",     0);
        bindMount("/home",  "/home",    Bind_ReadOnly);

        switchToBindRoot();

        char *ShellArguments[] = {
            "bash",
            0
        };

        char *Environment[] = {
            "HOME=/home/nox",
            0
        };

        if(execvpe(ShellArguments[0], ShellArguments, Environment) < 0)
        {
            fprintf(stderr, "Could not execute \"%s\"\n", ShellArguments[0]);
            exit(EXIT_FAILURE);
        }
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
