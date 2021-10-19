#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

read -rp "Configure PRIME? [y/N] " InstallPrime
if [[ "$InstallPrime" =~ ^[Yy]$ ]]
then
    ./install_prime.sh
fi

read -rp "Allow access to performance counters (developer mode)? [y/N] " InstallDevMode
if [[ "$InstallDevMode" =~ ^[Yy]$ ]]
then
    ./install_devmode.sh
fi

read -rp "Configure early module loading? [y/N] " InstallEarly
if [[ "$InstallEarly" =~ ^[Yy]$ ]]
then
    ./install_early.sh
fi
