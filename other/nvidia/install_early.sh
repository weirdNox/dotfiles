#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

ModulesToAdd="nvidia nvidia_modeset nvidia_uvm nvidia_drm"
if [[ $(grep -E "^[^#]+$ModulesToAdd" /etc/mkinitcpio.conf) ]]
then
    echo "It looks like the required NVIDIA modules are already present in the initramfs"
    echo "  ==> Check MODULES in /etc/mkinitcpio.conf"
else
    echo -n "Adding '$ModulesToAdd' to the initramfs... "
    sed -i "/^MODULES=.*/a MODULES+=($ModulesToAdd)" /etc/mkinitcpio.conf
    echo "done!"
fi

FilesToAdd=("/etc/udev/rules.d/80-nvidia-pm.rules")
for File in "${FilesToAdd[@]}"
do
    if [[ -f "$File" ]]
    then
        if [[ $(grep -E "^[^#]+$File" /etc/mkinitcpio.conf) ]]
        then
            echo "It looks like '$File' is already present in initramfs"
            echo "  ==> Check FILES in /etc/mkinitcpio.conf"
        else
            echo -n "Adding '$File' to the initramfs... "
            sed -i "/^FILES=.*/a FILES+=(\"$File\")" /etc/mkinitcpio.conf
            echo "done!"
        fi
    else
        echo "File $File doesn't exist... skipping insertion into initramfs!"
    fi
done

echo -n "Adding pacman hook... "
mkdir -p /etc/pacman.d/hooks/
cp -f nvidia.hook /etc/pacman.d/hooks/
echo "done!"
echo ""
echo "IMPORTANT: Possibly run 'mkinitcpio -P' now"
echo ""
