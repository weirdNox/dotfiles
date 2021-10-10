#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

ModulesToAdd="nvidia nvidia_modeset nvidia_uvm nvidia_drm"
if [[ $(grep -E "^[^#]+$ModulesToAdd" /etc/mkinitcpio.conf) ]]
then
    echo "It looks like the required NVIDIA modules are already present in the initcpio"
    echo "  ==> Check MODULES in /etc/mkinitcpio.conf"
else
    echo -n "Adding '$ModulesToAdd' to the initcpio... "
    sed -i "/^MODULES=.*/a MODULES+=($ModulesToAdd)" /etc/mkinitcpio.conf
    echo "done!"
fi

echo -n "Adding pacman hook... "
mkdir -p /etc/pacman.d/hooks/
cp -f nvidia.hook /etc/pacman.d/hooks/
echo "done!"
echo "IMPORTANT: Check if /etc/pacman.d/hooks/nvidia.hook lists the packages being used!"
