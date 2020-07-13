#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

cp "en_SE"                    "/usr/share/i18n/locales"
cp "locale.gen" "locale.conf" "/etc"

locale-gen
