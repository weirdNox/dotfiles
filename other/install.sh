#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

Folders=(container locale mime moltengamepad tmpfiles tty x11_input_daemon)
for Folder in "${Folders[@]}"
do
    "$Folder/install.sh" || true
done

echo -e "\n\n======> Done!"
