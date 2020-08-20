#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cp -vfas "$ScriptDir/container.h" "$XDG_BIN_HOME"
