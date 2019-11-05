#!/bin/bash
xrdb -merge ~/.Xdefaults
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps
nm-applet &
xscreensaver -nosplash &
xbindkeys

if [ -f ${HOME}/.xinitrc-local]; then
    . ${HOME}/.xinitrc-local
fi
trayer --edge top --align right --widthtype percent --width 5 --expand false --transparent true --alpha 1  --heighttype pixel --height 22 --tint 0x000000 --monitor 1 &

exec /usr/bin/xmonad 2>&1 > $HOME/.xmonad/log.txt
