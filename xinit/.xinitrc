#!/bin/bash
xrdb -merge ~/.Xdefaults

#xrandr --output DVI-I-1 --mode 2560x1440 --output DP-1 --mode 2560x1440 --right-of DVI-I-1 --rotate left
xsetroot -cursor_name left_ptr

#feh --bg-fill /home/frms/Pictures/december1994.jpg &
setxkbmap -option ctrl:nocaps
nm-applet &
xscreensaver -nosplash &

mate-volume-control-applet &
xbindkeys 

trayer --edge top --align right --widthtype percent --width 5 --expand false --transparent true --alpha 1  --heighttype pixel --height 22 --tint 0x000000 &

exec /usr/bin/xmonad 2>&1 > $HOME/.xmonad/log.txt

