#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

OutputDir="$HOME/.local/bin"
mkdir -p "$OutputDir"

g++ -O3 -g0 sfk.cpp sfkext.cpp sfkpack.cpp -o "$OutputDir/sfk"
