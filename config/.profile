#
# This file is loaded by many login shells, including graphical ones.
#

# Load environment.d
set -a; eval $(/lib/systemd/user-environment-generators/30-systemd-environment-d-generator); set +a

# Load .bashrc if this is interactive
[[ -n "$BASH_VERSION" && $- == *i* && -f "$HOME/.bashrc" ]] && source "$HOME/.bashrc"
