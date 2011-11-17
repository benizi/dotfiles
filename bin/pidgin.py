#!/usr/bin/env python
from subprocess import Popen

def do_not_play(event, account):
	print event, " and ", account
#	Popen(["audacious","/home/bhaskell/ripping/flac/Various Artists/Silver Screen Classics (disc 03)/01.flac"])
	if event == 3:
		Popen(["audacious","/home/bhaskell/ripping/flac/Various Artists/Silver Screen Classics (disc 03)/01.flac"])
	return False

def my_func(account, sender, message, conversation, flags):
	print sender, "said:", message

import dbus, gobject
from dbus.mainloop.glib import DBusGMainLoop
dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)
bus = dbus.SessionBus()

bus.add_signal_receiver(my_func,
	dbus_interface="im.pidgin.purple.PurpleInterface",
	signal_name="ReceivedImMsg")
bus.add_signal_receiver(do_not_play,
	dbus_interface="im.pidgin.purple.PurpleInterface",
	signal_name="PlayingSoundEvent")

loop = gobject.MainLoop()
loop.run()
