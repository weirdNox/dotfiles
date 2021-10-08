#!/usr/bin/tcc -run -Werror

#define HELPER_BINDS(W) W("/etc"), W("/home"), W("/usr")

#define KEEP_AUX 0

#define UNIONFS_CREATE 0
#define UNIONFS_UPPER

#define BIND_UID
#define BIND_GID
#define BIND_USER_NAME user
#define BIND_HOME_PATH
#define BIND_SHELL "/bin/bash"

#define SHARE_NETWORK 0
#define DUMMY_LINK    1
#define BIND_HOSTNAME host

#define SHARE_DBUS 0
#define DBUS_SYST_PROXY "--filter",
#define DBUS_USER_PROXY "--filter",

#define SHARE_CUDA 0

#define ENVS_TO_KEEP W("DISPLAY"), W("TERM")

#define BIND_ENV_DEFAULT_PATH "/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin"
#define PROC_VERSION

#define DROP_CAPS 0

#define ARGS_OVERRIDE_STRING "-"

#include "container.h"

CONFIGURE_CONTAINER()
{
    char *Rootfs = BaseUnionFS;
    bindRootfs(Rootfs, Rootfs_Minimal, Bind_ReadOnly);
    //bindHome(Rootfs, 0, 0);

    shareDisplay();
    shareGraphics();
    //setupMachineID("");
    //shareAudio(true);
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
