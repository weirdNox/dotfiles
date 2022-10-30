#
# This file is loaded by many login shells, including graphical ones.
#

# Load environment.d
set -a; eval $(/lib/systemd/user-environment-generators/30-systemd-environment-d-generator); set +a

# Create XDG Base Directories
dirs=()
for dir in XDG_{BIN,CACHE,CONFIG,DATA,LIB,STATE}_HOME; do dirs+=("${!dir}"); done
mkdir -p "${dirs[@]}"
unset dirs dir

# Load .bashrc if this is interactive
[[ -n "$BASH_VERSION" && $- == *i* && -f "$HOME/.bashrc" ]] && source "$HOME/.bashrc"
