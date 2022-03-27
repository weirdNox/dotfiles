#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

echo -n "Configuring DRM kernel mode setting... "
cp -f nvidia-drm-modesetting.conf /etc/modprobe.d/
echo "done!"
