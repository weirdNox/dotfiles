#!/usr/bin/env bash
set -euo pipefail; shopt -s nullglob
cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1

mkdir -p /etc/initcpio/{install,hooks}

cp encrypt_hook    /etc/initcpio/hooks/encrypt
cp encrypt_install /etc/initcpio/install/encrypt
