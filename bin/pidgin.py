#!/usr/bin/env python
from subprocess import Popen
timeout = "0" # stay forever

## def do_not_play(event, account):
## 	print event, " and ", account
## 	if event == 3:
## 		Popen(["audacious","/home/bhaskell/ripping/flac/Various Artists/Silver Screen Classics (disc 03)/01.flac"])
## 	return False

def my_func(account, sender, message, conversation, flags):
	print sender, "said:", message
	print "flags: ", flags

def alerter(account, sender, message, conversation, flags):
	# skip non-logged messages (replayed when rejoining chat)
	if flags & 1024:
		return
	nohtml = message.replace('<','&lt;').replace('>','&gt;')
	Popen(["envdir","/home/USC/bhaskell/dbus","notify-send","-t",timeout,sender,nohtml])
	print sender, "said:", message, "| flags:", flags

import dbus, gobject
from dbus.mainloop.glib import DBusGMainLoop
dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
bus = dbus.SessionBus()

bus.add_signal_receiver(alerter,
	dbus_interface="im.pidgin.purple.PurpleInterface",
	signal_name="ReceivedImMsg")

bus.add_signal_receiver(my_func,
	dbus_interface="im.pidgin.purple.PurpleInterface",
	signal_name="ReceivedChatMsg")

## bus.add_signal_receiver(do_not_play,
## 	dbus_interface="im.pidgin.purple.PurpleInterface",
## 	signal_name="PlayingSoundEvent")

loop = gobject.MainLoop()
print "Starting"
loop.run()
