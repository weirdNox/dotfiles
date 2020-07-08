#!/usr/bin/env sh
setxkbmap -layout us -variant intl -option ctrl:nocaps,shift:both_capslock,compose:rwin

for Device in "SynPS/2 Synaptics TouchPad" "Wacom Intuos PT S 2 Finger"; do
    if xinput list-props "$Device" >/dev/null 2>&1; then
        xinput set-prop "$Device" "libinput Tapping Enabled" 0
        xinput set-prop "$Device" "libinput Tapping Drag Enabled" 0
        xinput set-prop "$Device" "libinput Natural Scrolling Enabled" 1
    fi
done
