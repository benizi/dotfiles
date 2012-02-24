#!/usr/bin/python
# simple program for setting a window's icon dynamically
import gtk
import warnings
from sys import argv, exit
from struct import pack, unpack
from optparse import OptionParser

# parse some options
parser = OptionParser()
parser.add_option("--graphic", "--img", "--image", type="string",
	dest="imagefile", metavar="FILE",
	help="Image file for icon")
parser.add_option("--id", type="int", dest="window_id",
	help="Window ID whose icon is being set")
parser.add_option("-v","--verbose")
parser.add_option("-s","--silent","-q","--quiet")
(options, args) = parser.parse_args()

# allow image file and window ID to be given positionally
if not options.imagefile and len(args):
	options.imagefile = args[0]
	args = args[1:]
if not options.window_id and len(args):
	options.window_id = int(args[0], 0)
	args = args[1:]

# really simple error handling
def err(msg=None):
	if msg and not options.silent:
		print msg
	exit(1)

# use GDK to load a graphics file into an array of pixels
pixmap = gtk.gdk.pixbuf_new_from_file(options.imagefile)
if not pixmap:
	err("Couldn't load pixmap")
warnings.filterwarnings('ignore','PyArray_FromDimsAndDataAndDescr',DeprecationWarning)
pixels = pixmap.get_pixels_array()

# prop will contain an array of 32-bit integers
# starting with width, height, then pixel information
prop = []
prop += [len(pixels[0])]
prop += [len(pixels   )]

# pixel information is stored in the odd order: alpha, red, green, blue
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

# grab a handle to the window, then delete and reset its _NET_WM_ICON
window = gtk.gdk.window_foreign_new(options.window_id)
if not window:
	err("Couldn't open window")
window.property_delete("_NET_WM_ICON")
window.property_change("_NET_WM_ICON","CARDINAL",32,gtk.gdk.PROP_MODE_REPLACE,prop)

# many window managers need a hint that the icon has changed
# bits 2, 3, and 5 of the WM_HINTS flags int are, respectively:
# IconPixmapHint, IconWindowHint, and IconMaskHint
wmhints = window.property_get("WM_HINTS")
if wmhints:
	wmhints_type,wmhints_fmt,wmhints_tuple=wmhints
	wmhints_flags = wmhints_tuple[0]
	for member in [2,3,5]:
		if wmhints_flags & (1 << member):
			wmhints_flags ^= 1 << member
			wmhints_tuple[member+1] = 0
	window.property_change("WM_HINTS",wmhints_type,wmhints_fmt,gtk.gdk.PROP_MODE_REPLACE,wmhints_tuple)
