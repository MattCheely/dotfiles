#!/bin/sh

# Prints volume changes to stdout.
# Depends on inotify-tools package

SOUND_DEV="/dev/snd/controlC0"

volume() {
    statusln=`amixer -D default sget Master,0 \
        | grep dB \
        | head -n 1`

    if echo $statusln | grep -q "\[on\]"
    then
        echo $statusln | cut -f 4 -d " " | sed -e's/\[//g' -e 's/\]//g'
    else
        echo ---
    fi
}

# loop only runs when inotify didn't fail (not present on all systems)
# and when parent process is xmobar. this _should_ ensure script quits after xmonad resets
while [ $? -eq 0 ] && [[ $(ps p $PPID | grep xmobar) ]];
do
    volume
    inotifywait $SOUND_DEV -e ACCESS -e CLOSE_WRITE > /dev/null 2>/dev/null
done

# also check PID of parent. if that's gone or not xmobar, bail
