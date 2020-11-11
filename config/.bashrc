# This file is automatically loaded by _interactive non-login_ bash shells.
# For _interactive login_ bash shells, ~/.profile loads this.

# Don't insert commands that start with a space in the history
export HISTCONTROL=ignorespace

# ------------------------------------------------------------------------------------------
# Aliases
alias   ls='  ls --color=auto'
alias grep='grep --color=auto'

alias vpn-up='  wg-quick up   mullvad'
alias vpn-down='wg-quick down mullvad'

alias moltengamepad='moltengamepad --mimic-xpad --dpad-as-hat'

# This makes bash check if the commands following sudo are aliases
alias sudo='sudo '
