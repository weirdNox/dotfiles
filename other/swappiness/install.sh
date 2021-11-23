#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"

cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

echo "Reducing swappiness"
mkdir -p /etc/sysctl.d
cp 99-swappiness.conf /etc/sysctl.d/
