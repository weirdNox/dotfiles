#!/usr/bin/env bash
ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$ScriptDir"

cp -vfasT "$ScriptDir/config/" "$HOME/"
./programs/build.sh
