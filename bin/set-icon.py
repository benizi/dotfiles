#!/usr/bin/python
import pygtk
import gtk
import warnings
from sys import argv, exit
from struct import pack, unpack
from optparse import OptionParser

parser = OptionParser()
parser.add_option("--graphic", "--img", "--image", type="string",
	dest="imagefile", metavar="FILE",
	help="Image file for icon")
parser.add_option("--id", type="int", dest="window_id",
	help="Window ID whose icon is being set")
parser.add_option("-v","--verbose")
parser.add_option("-s","--silent","-q","--quiet")
(options, args) = parser.parse_args()
if not options.imagefile and len(args):
	options.imagefile = args[0]
	args = args[1:]
if not options.window_id and len(args):
	options.window_id = int(args[0], 0)
	args = args[1:]

verbose = False
silent = True
def err(msg=None):
	if msg and not options.silent:
		print msg
	exit(1)

pixmap = gtk.gdk.pixbuf_new_from_file(options.imagefile)
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

window = gtk.gdk.window_foreign_new(options.window_id)
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
