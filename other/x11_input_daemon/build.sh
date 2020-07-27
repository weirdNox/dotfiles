#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob

ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OutputDir="$HOME/.local/bin"

Cmd=(gcc)

Cmd+=(
    -Wall -Wextra
    -Wno-unused-parameter
)

Cmd+=(
    -O3 -g0
)

Cmd+=(
    -lxcb
    -lxcb-xinput
)

Cmd+=(
    x11_input_daemon.c
    -o "$OutputDir/x11_input_daemon"
)

mkdir -p "$OutputDir"
cd "$ScriptDir"
"${Cmd[@]}"
