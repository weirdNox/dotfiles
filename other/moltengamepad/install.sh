#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

cd "$ScriptDir"
mkdir -p build

cd build
ln -sf "$ScriptDir/PKGBUILD" PKGBUILD
makepkg -si --clean --noconfirm --needed

sudo udevadm control --reload
