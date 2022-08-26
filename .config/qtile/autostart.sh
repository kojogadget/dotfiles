#!/usr/bin/env bash

export XAUTHORITY=/home/kg/.Xauthority
/usr/lib/pam_kwallet_init

picom &
xset s on
xset s 150 600
xss-lock -n "changebrightness down" -- i3lock -c 000000 -n &
# stalonetray &

nm-applet &
blueman-applet &
pa-applet &
kdeconnect-indicator &
optimus-manager-qt &
dunst &
nextcloud &
/usr/bin/emacs --daemon &
autorandr -c
feh --randomize --bg-fill /home/kg/Pictures/backgrounds/*
