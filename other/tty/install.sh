#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

mkdir -p /usr/local/share/kbd/keymaps

cp custom.map    /usr/local/share/kbd/keymaps
cp vconsole.conf /etc
