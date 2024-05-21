#
# This file is automatically loaded by interactive non-login Bash shells.
# For interactive login bash shells, ~/.profile loads this.
#

# Disable XON/XOFF flow control (allows using Ctrl-s)
stty -ixon

# History settings
HISTCONTROL=ignoreboth # Don't insert commands that start with a space or that are duplicates in the history
HISTSIZE=-1            # Unlimited history during a single session
HISTFILESIZE=5000000   # Number of commands to store in history file
HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/bash_history"
mkdir -p "$(dirname -- "$HISTFILE")"

# ------------------------------------------------------------------------------------------
# Aliases and helper functions
alias    ls='ls --color=auto'
alias    ll='ls -la'
alias    l.='ls -d .* --color=auto'
alias  grep='grep --color=auto'
alias mount='mount | column -t'

alias moltengamepad='moltengamepad --mimic-xpad --dpad-as-hat'

alias   gdb='gdb -q'
alias gdash='gdb -q -x "$XDG_CONFIG_HOME"/gdb/gdb-dashboard'
alias   gef='gdb -q -x "$XDG_CONFIG_HOME"/gdb/gef.py'

# This makes bash check if the commands following sudo are aliases
alias sudo='sudo '

function watch_and_run {
    local args=(); while [[ $# -ge 1 && "$1" != "--" ]]; do args+=("$1"); shift 1; done
    shift 1

    if [[ "${#args}" -lt 1 || ! $# -eq 1 ]]; then
        echo "USAGE: ${FUNCNAME[0]} <arg> [<arg> ...] -- <cmd>"
        return 1
    fi

    local batch_window="${batch_window:-5}"

    IFS= inotifywait -e create,close_write,moved_to --format '%w%f%0' -mr "${args[@]}" 2>/dev/null |
        while read -d '' file; do
            local files=("$file")
            while read -d '' -t "$batch_window" file; do files+=("$file"); done
            readarray -td '' files < <(printf "%s\0" "${files[@]}" | sort -zu)
            eval "$@"
        done
}
