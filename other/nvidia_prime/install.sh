#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

pacman -Syu --needed nvidia-prime
sudo systemctl enable nvidia-persistenced

echo -n "Configuring NVIDIA power management... "
cp    nvidia-pm.conf  /etc/modprobe.d/
cp 80-nvidia-pm.rules /etc/udev/rules.d/
echo "done!"

read -rp "Configure early module loading? [y/N] " InstallEarly
if [[ "$InstallEarly" =~ ^[Yy]$ ]]
then
    ./install_early.sh
fi
