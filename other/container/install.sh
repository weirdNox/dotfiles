#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

echo "Setting up container software"
: "${XDG_BIN_HOME:=$HOME/.local/bin}"
mkdir -p "$XDG_BIN_HOME"


sudo mkdir -p "/usr/local/include/nox"
sudo cp -vfas "$(pwd)/container_base.h" "$(pwd)/container_common.h" "/usr/local/include/nox"

./build_helpers.sh
sudo mkdir -p "/usr/local/lib/nox"
sudo mv -v "$(pwd)/fake_sudo" "/usr/local/lib/nox/"
