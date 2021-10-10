#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
cd "$(dirname "${BASH_SOURCE[0]}")"

rm -rf build
mkdir -p build
cd build

ln -sf "$(pwd)/../PKGBUILD" PKGBUILD
makepkg -si --noconfirm --needed

# NOTE(nox): Ignore changes from makepkg
sed -i 's/^pkgver=.*/pkgver=0/' ../PKGBUILD
