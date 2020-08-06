#!/usr/bin/env bash
# NOTE(nox): Only for rapidly checking if there are errors in the program

set -euo pipefail; shopt -s nullglob

ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ScriptDir"

Cmd=(clang)

Cmd+=(
    -ggdb

    -Wall -Wextra
    -Wno-unused-parameter
)

Cmd+=(container.c)

"${Cmd[@]}"
