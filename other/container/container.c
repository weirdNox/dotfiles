#!/usr/bin/tcc -run

   // TODO(nox): Make struct for holding configuration

#define UnshareNet 1

#define BindUser "user"
#define BindHome "/home/"BindUser
#define BindUID -1
#define BindGID BindUID

#define HostName "host"

#include "container.h"

CONFIGURE_CONTAINER()
{
    setenv("HOME", "/home/nox", true);
    setenv("PATH", "/usr/local/sbin:/usr/local/bin:/usr/bin", true);

    bindMount("/home",  "/home",  Bind_ReadOnly);

    bindMount("/etc",   "/etc",   0);
    bindMount("/lib",   "/lib",   0);
    bindMount("/lib64", "/lib64", 0);
    bindMount("/sys",   "/sys",   0);
    bindMount("/usr",   "/usr",   0);

    otherMount(Mount_Dev,  "/dev");
    otherMount(Mount_Proc, "/proc");
}

RUN_COMMAND()
{
    char *ShellArguments[] = {
        "bash",
        0
    };
    execute(ShellArguments);
}

// Local Variables:
// mode: c
// End:
