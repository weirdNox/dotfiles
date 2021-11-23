#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"

cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

echo "Disabling tmpfs on /tmp"
systemctl mask tmp.mount

echo "Making sure /tmp and /var/tmp are created and cleaned up"
mkdir -p /etc/tmpfiles.d
cp tmp.conf /etc/tmpfiles.d/

echo "Enabling paccache to prevent unbounded cache growth"
systemctl enable --now paccache.timer

echo "Setting journal size limit"
mkdir -p /etc/systemd/journald.conf.d
cp 00-journal-size.conf /etc/systemd/journald.conf.d/
