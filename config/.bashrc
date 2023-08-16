#
# This file is automatically loaded by interactive non-login Bash shells.
# For interactive login bash shells, ~/.profile loads this.
#

# History settings
HISTCONTROL=ignoreboth # Don't insert commands that start with a space or that are duplicates in the history
HISTSIZE=-1            # Unlimited history during a single session
HISTFILESIZE=5000000   # Number of commands to store in history file
HISTFILE="$XDG_STATE_HOME/bash_history"

# ------------------------------------------------------------------------------------------
# Aliases
alias   ls='ls   --color=auto'
alias grep='grep --color=auto'

alias   vpn-up='wg-quick up   mullvad'
alias vpn-down='wg-quick down mullvad'

alias moltengamepad='moltengamepad --mimic-xpad --dpad-as-hat'

alias   gdb='gdb -q'
alias gdash='gdb -q -x "$XDG_CONFIG_HOME"/gdb/gdb-dashboard'
alias   gef='gdb -q -x "$XDG_CONFIG_HOME"/gdb/gef.py'

# This makes bash check if the commands following sudo are aliases
alias sudo='sudo '
