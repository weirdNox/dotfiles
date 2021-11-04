#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

echo -n "Adding pacman hooks... "
mkdir -p /etc/pacman.d/hooks/
cp -f kernmodbak-{pre,post}.hook /etc/pacman.d/hooks/
echo "done!"
