#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

setxkbmap -model pc104 -option ctrl:nocaps
xsetroot -solid "#2b2b2b" -cursor_name left_ptr
feh --randomize --bg-fill /usr/share/backgrounds/* &
xscreensaver -no-splash&

exec xmonad
# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
