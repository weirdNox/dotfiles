#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>
#include <xcb/xcb.h>
#include <xcb/xinput.h>

static void printError_(const char *Format, va_list Args) {
    fprintf(stderr, "[x11_input_daemon] ");
    vfprintf(stderr, Format, Args);
}

static void printError(const char *Format, ...) {
    va_list Args;
    va_start(Args, Format);
    printError_(Format, Args);
    va_end(Args);
}

static void errorAndDie(const char *Format, ...) {
    va_list Args;
    va_start(Args, Format);
    printError_(Format, Args);
    va_end(Args);

    exit(1);
}

static void catchChild(int Sig) {
    waitpid(-1, 0, 0);
}

static int getDeviceCount(xcb_connection_t *Conn) {
    int DeviceCount = -1;
    xcb_input_list_input_devices_cookie_t Cookie = xcb_input_list_input_devices(Conn);

    xcb_generic_error_t *Err;
    xcb_input_list_input_devices_reply_t *Reply = xcb_input_list_input_devices_reply(Conn, Cookie, &Err);
    if(Reply) {
        DeviceCount = xcb_input_list_input_devices_devices_length(Reply);
        free(Reply);
    }
    else {
        printError("Failed to list input devices. Error code: %d\n", Err->error_code);
        free(Err);
    }

    return DeviceCount;
}

static void executeScript(char *ArgVals[]) {
    switch(fork()) {
        case -1: {
            printError("Could not fork in order to execute the script\n");
        } break;

        case 0: {
            if(execvp(ArgVals[0], ArgVals) < 0) {
                errorAndDie("Failed to execute provided program (%s): %s\n", ArgVals[0], strerror(errno));
            }
        } break;

        default: break;
    }
}

static void setupDeviceMask(xcb_connection_t *Conn, xcb_screen_t *Screen) {
    struct {
        xcb_input_event_mask_t Head;
        xcb_input_xi_event_mask_t Mask;
    } Mask;

    Mask.Head.deviceid = XCB_INPUT_DEVICE_ALL;

    Mask.Mask = XCB_INPUT_XI_EVENT_MASK_HIERARCHY;
    Mask.Head.mask_len = sizeof(Mask.Mask) / sizeof(uint32_t);
    xcb_input_xi_select_events(Conn, Screen->root, 1, &Mask.Head);
    xcb_flush(Conn);
}

int main(int ArgCount, char *ArgVals[]) {
    if(ArgCount < 2) {
        fprintf(stderr, "USAGE: %s [Command [Args...]]\n", ArgVals[0]);
        return 1;
    }

    uid_t Uid = getuid();
    if(Uid == 0 || Uid != geteuid()) {
        errorAndDie("This should not run as root\n");
    }

    switch(fork()) {
        case -1: {
            errorAndDie("Could not fork\n");
        } break;

        case 0: {} break;

        default: {
            return 0;
        } break;
    }

    setsid();

    close(STDIN_FILENO);
    close(STDERR_FILENO);
    close(STDOUT_FILENO);

    signal(SIGCHLD, catchChild);

    xcb_connection_t *Conn = xcb_connect(0, 0);
    if(xcb_connection_has_error(Conn)) {
        errorAndDie("Failed to get a connection to the X server");
    }

    xcb_screen_t *Screen = xcb_setup_roots_iterator(xcb_get_setup(Conn)).data;
    setupDeviceMask(Conn, Screen);

    executeScript(ArgVals+1);

    int LastDeviceCount = 0;
    for(xcb_generic_event_t *Event; (Event = xcb_wait_for_event(Conn));) {
        int DeviceCount = getDeviceCount(Conn);

        if(DeviceCount != LastDeviceCount) {
            LastDeviceCount = DeviceCount;
            executeScript(ArgVals+1);
        }

        free(Event);
    }

    xcb_disconnect(Conn);

    return 0;
}
