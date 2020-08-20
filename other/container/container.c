#!/usr/bin/tcc -run

#define BIND_UID
#define BIND_GID
#define BIND_USER_NAME user
#define BIND_HOME_PATH

#define SHARE_NETWORK 0

#define SHARE_DBUS    0
#define DBUS_SYST_PROXY "--filter",
#define DBUS_USER_PROXY "--filter",

#define ENVS_TO_KEEP W("DISPLAY"), W("TERM")

#define BIND_HOSTNAME host
#define BIND_ENV_DEFAULT_PATH "/usr/local/sbin:/usr/local/bin:/usr/bin"

#define ARGS_OVERRIDE_STRING "-"

#include "container.h"

CONFIGURE_CONTAINER()
{
    char *Rootfs = 0;
    bindRootfs(Rootfs, false, Bind_ReadOnly);

    //bindHome(Rootfs, 0, 0);

    shareDisplay();
    shareGraphics();
    //shareAudio();
    //shareInput();

    //bindCustomWineBuild("...")
}

RUN_COMMAND()
{
    char *ShellArguments[] = {"bash"};
    execute(ArgCount, ArgVals, ShellArguments);
}

// Local Variables:
// mode: c
// End:
