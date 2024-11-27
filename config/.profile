#
# This file is loaded by many login shells, including graphical ones.
#

# Load environment.d
set -o allexport
source <(/usr/lib/systemd/user-environment-generators/30-systemd-environment-d-generator)
set +o allexport

# Create XDG Base Directories
dirs=()
for dir in XDG_{BIN,CACHE,CONFIG,DATA,LIB,STATE}_HOME; do dirs+=("${!dir}"); done
mkdir -p "${dirs[@]}"
unset dirs dir

# Xorg settings (also useful for Xwayland)
xrdb -merge ~/.config/X11/Xresources       >/dev/null 2>&1 || :
xrdb -merge ~/.config/X11/Xresources_local >/dev/null 2>&1 || :
xhost +local:                              >/dev/null 2>&1 || :

# Load .bashrc if this is interactive
[[ -n "$BASH_VERSION" && $- == *i* && -f "$HOME/.bashrc" ]] && source "$HOME/.bashrc"
