#!/usr/bin/env sh
OutputDir="$HOME/.local/bin"
Common="-Wall -Wextra -Wno-unused-parameter -O3 -g0"

cd "$(dirname "${BASH_SOURCE[0]}")"
mkdir -p $OutputDir

gcc $Common -lxcb -lxcb-xinput input_daemon.c -o "$OutputDir/input_daemon"
