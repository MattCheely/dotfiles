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
#TODO: Generate list of files instead of using folders & remove .desktop entries
feh --randomize --recursive --bg-fill ~/.backgrounds/ &
xscreensaver -no-splash&

sxhkd &
exec bspwm

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
