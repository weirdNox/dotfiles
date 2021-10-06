#!/usr/bin/env bash
set -euo pipefail

# NOTE(nox): Keyboard
setxkbmap -layout us -variant intl -option ctrl:nocaps,shift:both_capslock,compose:rwin

# NOTE(nox): Touchpads
readarray -t Touchpads <<< $(xinput list --name-only | grep -i touchpad)
Touchpads+=("Wacom Intuos PT S 2 Finger")

for Device in "${Touchpads[@]}"; do
    if xinput list-props "$Device" >/dev/null 2>&1; then
        xinput set-prop "$Device" "libinput Tapping Enabled" 0
        xinput set-prop "$Device" "libinput Tapping Drag Enabled" 0
        xinput set-prop "$Device" "libinput Natural Scrolling Enabled" 1
    fi
done
