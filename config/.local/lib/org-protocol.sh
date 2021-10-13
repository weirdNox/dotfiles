#!/usr/bin/env sh
emacsclient  -a "" -e "(require 'org-protocol)"
emacsclient -ca "" -F '((name . "Org Capture")(org-capture-frame . t)(width . 110)(height . 40))' "$@"
