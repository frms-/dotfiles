#!/bin/bash
xrdb -merge ~/.Xdefaults
xsetroot -cursor_name left_ptr
setxkbmap -option ctrl:nocaps
nm-applet &
volumeicon &
xscreensaver -nosplash &
xbindkeys

if [ -f "$HOME"/.xinitrc-local ] ; then
    # shellcheck source=./.xinitrc-local
    source "$HOME"/.xinitrc-local
fi

TWIDTH=${TRAYER_WITH:-5}
THEIGHT=${TRAYER_HEIGHT:-22}

trayer --edge top --align right --widthtype percent --width "$TWIDTH" --expand false --transparent true --alpha 1  --heighttype pixel --height "$THEIGHT" --tint 0x000000 --monitor 1 &

export XMONAD_GHC=/usr/bin/ghc
echo "XMONAD_GHC: $XMONAD_GHC" > "$HOME"/log.txt
exec /usr/bin/xmonad > "$HOME"/.xmonad/log.txt 2>&1
