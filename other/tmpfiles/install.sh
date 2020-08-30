#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"

cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

systemctl mask tmp.mount

mkdir -p /etc/tmpfiles.d
cp tmp.conf /etc/tmpfiles.d
