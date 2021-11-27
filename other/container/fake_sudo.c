#include "container_common.h"

int main(int ArgCount, char *ArgVals[])
{
    int ProgramResult = 0;

    --ArgCount;
    ++ArgVals;

    uid_t BaseUID = geteuid();
    gid_t BaseGID = getegid();

    sigset_t SigMask;
    sigemptyset(&SigMask); sigaddset(&SigMask, SIGUSR1);
    sigprocmask(SIG_BLOCK, &SigMask, 0);

    pid_t ChildPID = cloneSC(SIGCHLD | CLONE_NEWUSER);

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

        mapID(ChildPID, Map_UID, BaseUID, 0);
        mapID(ChildPID, Map_GID, BaseGID, 0);

        kill(ChildPID, SIGUSR1);

        int WaitStatus = 0;
        waitpid(ChildPID, &WaitStatus, 0);
        ProgramResult = WEXITSTATUS(WaitStatus);
    }
    else if(ChildPID == 0)
    {
        // NOTE(nox): Child
        while(sigwaitinfo(&SigMask, 0) < 0);
        sigprocmask(SIG_UNBLOCK, &SigMask, 0);

        keepCaps();

        while(ArgCount > 0 && *ArgVals[0] == '-')
        {
            s32 Advance = 1;
            char Letter = *(ArgVals[0] + 1);

            switch(Letter)
            {
                case 'C': case 'D': case 'R': case 'T': case 'U':
                case 'g': case 'h': case 'p': case 'u':
                {
                    Advance = 2;
                } break;

                default: {} break;
            }

            ArgCount -= Advance;
            ArgVals  += Advance;
        }

        if(ArgCount > 0)
        {
            ProgramResult = (execvp(ArgVals[0], ArgVals) < 0 ? 1 : 0);
        }
    }
    else
    {
        // NOTE(nox): Error while cloning
        fprintf(stderr, "Could not clone: %s\n", strerror(errno));
        exit(EXIT_FAILURE);
    }

    return ProgramResult;
}
// Local Variables:
// mode: c
// End:
