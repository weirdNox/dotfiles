#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$ScriptDir"
mkdir -p build

cd build
ln -sf "$ScriptDir/PKGBUILD" .
ln -sf "$ScriptDir/pipewire-jack.conf" .
makepkg -si --clean