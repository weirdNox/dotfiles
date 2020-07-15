#!/usr/bin/env sh
# This file is loaded by many _login_ shells, including graphical ones.

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        [[ $- == *i* ]] && . "$HOME/.bashrc"
    fi
fi

if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 && -f "$HOME/.xinitrc" ]]; then
    exec startx
fi
