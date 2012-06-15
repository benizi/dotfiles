CFLAGS := -Wall
SRC = src
allsource = $(wildcard $(SRC)/*.c)

all:	$(notdir $(basename $(allsource)))

place-holder:	src/place-holder.c
	$(CC) $(CFLAGS) -o $@.tmp $< -lcurses
	mv $@.tmp $@

%:	src/%.c
	$(CC) $(CFLAGS) -o $@.tmp $<
	mv $@.tmp $@

%:	src/X11/%.c
	$(CC) $(CFLAGS) -I$(HOME)/include $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

x-active-id:	src/X11/x-active-id.c
	$(CC) $(CFLAGS) -o $@.tmp $< $(shell pkg-config --cflags --libs xcb-atom)
	mv $@.tmp $@

x-atom-name:	src/X11/x-atom-name.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

x-xinerama-info:	src/X11/x-xinerama-info.c
	$(CC) $(CFLAGS) -o $@.tmp $< $(shell pkg-config --cflags --libs xcb-xinerama)
	mv $@.tmp $@

x-is-active:	src/X11/x-is-active.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

x-xinerama-info-xlib:	src/X11/x-xinerama-info-xlib.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11) -lXinerama
	mv $@.tmp $@

x-ungrab:	src/X11/x-ungrab.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

x-resource-test:	src/X11/x-resource-test.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

NOW:	src/NOW.c
	$(CC) $(CFLAGS) -I../include -L../lib -o $@.tmp $< -lmyc
	mv $@.tmp $@

time-limit:	src/time-limit.c
	$(CC) $(CFLAGS) -I../include -L../lib -o $@.tmp $< -lmyc
	mv $@.tmp $@

echo:
	echo $(basename $(allsource))
