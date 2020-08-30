#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob

ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ScriptDir"

cp -vfasT "$ScriptDir/config/" "$HOME/"

mkdir -p "$HOME/temporary/"{downloads,other/desktop}
