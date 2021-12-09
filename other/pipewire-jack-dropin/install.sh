#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
ScriptDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Installing pipewire-jack-dropin"

cd "$ScriptDir"
mkdir -p build

cd build
ln -sf "$ScriptDir/PKGBUILD" .
ln -sf "$ScriptDir/pipewire-jack.conf" .
makepkg --syncdeps --clean --force
sudo pacman -U --noconfirm *.pkg.* || sudo pacman -U *.pkg.*
