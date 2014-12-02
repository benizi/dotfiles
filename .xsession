# vim: se ft=zsh: -*- mode: zsh; -*-
if test -z "$ZSH_VERSION" && test -e /bin/zsh ; then exec /bin/zsh $0 ; fi

# TODO: not sure why these aren't being pulled in by exec'ing zsh
export LANG=en_US.UTF-8 LC_COLLATE=POSIX

exec 2> ~/.wm.errors

keyboard

xrdb ~/.Xdefaults

if (( $+commands[dbus-launch] )) ; then
  if ! (( $+DBUS_SESSION_BUS_PID )) || ! kill -0 $DBUS_SESSION_BUS_PID ; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
  fi
fi

xsetroot -solid black

exec xmonad
