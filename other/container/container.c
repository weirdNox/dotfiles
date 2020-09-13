#!/usr/bin/tcc -run -Werror

#define KEEP_AUX 0

#define UNIONFS_CREATE 0
#define UNIONFS_UPPER

#define BIND_UID
#define BIND_GID
#define BIND_USER_NAME user
#define BIND_HOME_PATH

#define SHARE_NETWORK 0
#define DUMMY_LINK    0
#define BIND_HOSTNAME host

#define SHARE_DBUS 0
#define DBUS_SYST_PROXY "--filter",
#define DBUS_USER_PROXY "--filter",

#define ENVS_TO_KEEP W("DISPLAY"), W("TERM")

#define BIND_ENV_DEFAULT_PATH "/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin"
#define PROC_VERSION

#define ARGS_OVERRIDE_STRING "-"

#include "container.h"

CONFIGURE_CONTAINER()
{
    char *Rootfs = BaseUnionFS;
    bindRootfs(Rootfs, Rootfs_Minimal, Bind_ReadOnly);
    //bindHome(Rootfs, 0, 0);

    shareDisplay();
    shareGraphics();
    //shareAudio();
    //shareInput();

    //bindCustomWineBuild("...");
}

RUN_COMMAND()
{
    char *ShellArguments[] = {"bash"};
    execute(ArgCount, ArgVals, ShellArguments);
}

// Local Variables:
// mode: c
// End:
