#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

mkdir -p ~/.config ~/.local/share/mime

cp mimeapps.list ~/.config
update-mime-database ~/.local/share/mime
