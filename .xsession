# vim: se ft=zsh: -*- mode: zsh; -*-
if test -z "$ZSH_VERSION" && test -e /bin/zsh ; then exec /bin/zsh $0 ; fi

# TODO: not sure why these aren't being pulled in by exec'ing zsh
export LANG=en_US.UTF-8 LC_COLLATE=POSIX

exec 2> ~/.wm.errors

keyboard

xrdb ~/.Xdefaults

# If no DBus session is present
if (( ! $+DBUS_SESSION_BUS_ADDRESS ))
then
  # Check for a systemd-style socket
  sock=/run/user/$UID/bus
  if [[ -e ${sock:h} ]]
  then export DBUS_SESSION_BUS_ADDRESS=unix:path=$sock
  elif (( $+commands[dbus-launch] ))
  then
    # or launch a session manually if it's not present or if it's dead
    if (( ! $+DBUS_SESSION_BUS_PID )) || ! kill -0 $DBUS_SESSION_BUS_PID
    then eval "$(dbus-launch --sh-syntax --exit-with-session)"
    fi
  fi
fi

xsetroot -solid black

dbus-update-activation-environment --systemd DISPLAY

exec xmonad
