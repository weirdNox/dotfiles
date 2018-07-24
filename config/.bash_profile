# This file is loaded by *all* bash login shells.

. ~/.profile

# NOTE(nox): Load bashrc if interactive
[[ $- == *i* ]] && . ~/.bashrc
