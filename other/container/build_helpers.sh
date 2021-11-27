#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

gcc()
{
    local Cmd=("${FUNCNAME[0]}" "$@")
    echo "${Cmd[@]}"
    command "${Cmd[@]}"
}

FakeSudoFlags+=(
    -O3 -ggdb3 -static

    -fno-strict-overflow
    -fno-strict-aliasing
    -fno-delete-null-pointer-checks

    -fno-trapping-math
    -fno-math-errno
    -fno-signed-zeros

    -fno-exceptions

    -Wall -Wextra
    -Wshadow

    -Wconversion
    -Wfloat-conversion
    -Wno-sign-conversion

    -Wno-missing-braces
    -Wno-missing-field-initializers
    -Wno-unused-const-variable
    -Wno-unused-function
    -Wno-write-strings
)

gcc "${FakeSudoFlags[@]}" "$(pwd)/fake_sudo.c" -o fake_sudo
