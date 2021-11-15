#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
[ "$(whoami)" = root ] || exec sudo -p "$(printf "This command needs to run as root.\nPassword: ")" $0 "$@"
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

echo "Setting up locale"
cp "en_SE"                    "/usr/share/i18n/locales"
cp "locale.gen" "locale.conf" "/etc"
locale-gen
