#!/usr/bin/python
import pygtk
import gtk
import warnings
from sys import argv, exit
from struct import pack, unpack
import getopt

verbose = False
silent = True
def err(msg=None):
	if msg and not silent:
		print msg
	exit(1)

if len(argv) < 3:
	err("Usage: %s icon-file window-id" % (argv[0]))

pixmap = gtk.gdk.pixbuf_new_from_file(argv[1])
if not pixmap:
	err("pixmap")

warnings.filterwarnings('ignore','PyArray_FromDimsAndDataAndDescr',DeprecationWarning)
pixels = pixmap.get_pixels_array()

prop = []
prop += [len(pixels[0])] #unpack("BBBB", pack("I", len(pixels[0]))) # width
prop += [len(pixels   )] #unpack("BBBB", pack("I", len(pixels   ))) # height

# print "Image %s loaded (%sx%s)" % (argv[1], prop[0],prop[1])
for row in pixels:
	for p in row:
		p = p.tolist()
		if len(p) < 4:
			p += [255]
		p = p[3:] + p[:-1]
		argb = int(0)
		for i in range(len(p)):
			argb += p[i] << (8 * (len(p)-i-1))
		prop += unpack("i",pack("I",argb))

window = gtk.gdk.window_foreign_new(int(argv[2],16))
if not window:
	err("Couldn't open window")
window.property_delete("_NET_WM_ICON")
window.property_change("_NET_WM_ICON","CARDINAL",32,gtk.gdk.PROP_MODE_REPLACE,prop)

wmhints = window.property_get("WM_HINTS")
if wmhints:
	wmhints_type,wmhints_fmt,wmhints_tuple=wmhints
	wmhints_flags = wmhints_tuple[0]
	for member in [2,3,5]:
		if wmhints_flags & (1 << member):
			wmhints_flags ^= 1 << member
			wmhints_tuple[member+1] = 0
	window.property_change("WM_HINTS",wmhints_type,wmhints_fmt,gtk.gdk.PROP_MODE_REPLACE,wmhints_tuple)
