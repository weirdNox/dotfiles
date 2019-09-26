#!/usr/bin/env sh
# This file is loaded by many _login_ shells, including graphical ones.

export PATH="$HOME/.local/bin:$PATH"

export EDITOR="$HOME/.local/bin/editor"
export VISUAL="$EDITOR"
export ALTERNATE_EDITOR=""

export GOPATH="$HOME/Work/Go"
export MATLAB_LOG_DIR="/tmp"

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        [[ $- == *i* ]] && . "$HOME/.bashrc"
    fi
fi
