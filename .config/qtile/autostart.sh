#!/usr/bin/env bash

export XAUTHORITY=/home/kg/.Xauthority

/usr/lib/pam_kwallet_init
picom &
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
