#!/usr/bin/env python
import dbus
from xml.etree import ElementTree

bluez_bus = 'org.bluez'
bluez_root = '/org/bluez'

intro_iface = 'org.freedesktop.DBus.Introspectable'
props_iface = 'org.freedesktop.DBus.Properties'
dev_iface = 'org.bluez.Device1'

a2dp_uuid = '0000110d-0000-1000-8000-00805f9b34fb'
avrcp_uuid = '0000110e-0000-1000-8000-00805f9b34fb'

svc_audio = int('200000', 16)
dev_major_av = int('400', 16)
dev_minor_headset = int('4', 16)
dev_minor_headphone = int('18', 16)


def btobj(path):
    return dbus.SystemBus().get_object(bluez_bus, path)


# from: https://unix.stackexchange.com/a/203678/2582 (kind of)
def kids(obj):
    prefix = obj.object_path.rstrip('/')
    for node in intro(obj, 'node'):
        yield "%s/%s" % (prefix, node)


def interfaces(obj):
    return list(intro(obj, 'interface'))


def intro(obj, tag=None):
    iface = dbus.Interface(obj, intro_iface)
    tree = ElementTree.fromstring(iface.Introspect())
    if not tag:
        return tree
    return [e.attrib['name'] for e in tree if e.tag == tag]


def devprop(obj, name, default=None):
    return prop(obj, dev_iface).get(name, default)


def prop(obj, iface, attr=None):
    attrs = obj.GetAll(iface, dbus_interface=props_iface)
    if attr:
        return attrs.get(attr)
    return attrs


def uuids(obj):
    return devprop(obj, 'UUIDs', [])


def hasbits(n, bits):
    return (n & bits) == bits


def is_headphones(obj):
    cls = devprop(obj, 'Class', 0)
    if not hasbits(cls, svc_audio):
        return False
    if not hasbits(cls, dev_major_av):
        return False
    return hasbits(cls, dev_minor_headset) or hasbits(cls, dev_minor_headphone)


bluez = btobj(bluez_root)
hci = btobj(list(kids(bluez))[0])
devs = list(kids(hci))
headphones = [d for d in devs if is_headphones(btobj(d))]
bth = btobj(headphones[0])
print(devprop(bth, 'Address'))
