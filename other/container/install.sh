#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Setting up container software"
: "${XDG_BIN_HOME:=$HOME/.local/bin}"
mkdir -p "$XDG_BIN_HOME"
cp -vfas "$ScriptDir/container.h" "$XDG_BIN_HOME"
