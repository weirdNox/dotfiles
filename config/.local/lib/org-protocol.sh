#!/usr/bin/env sh
emacsclient -e "(require 'org-protocol)"
emacsclient -c -F '((name . "Org Capture")(org-capture-frame . t)(width . 110)(height . 40))' "$@"
