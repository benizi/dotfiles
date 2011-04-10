SRC = src
allsource = $(wildcard $(SRC)/*.c)

all:	$(notdir $(basename $(allsource)))

place-holder:	src/place-holder.c
	gcc -Wall -o $@.tmp $< -lcurses
	mv $@.tmp $@

%:	src/%.c
	gcc -Wall -o $@.tmp $<
	mv $@.tmp $@

%:	src/X11/%.c
	gcc -Wall -I$(HOME)/include $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

x-active-id:	src/X11/x-active-id.c
	gcc -Wall -o $@.tmp $< $(shell pkg-config --cflags --libs xcb-atom)
	mv $@.tmp $@

x-is-active:	src/X11/x-is-active.c
	gcc -Wall $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

NOW:	src/NOW.c
	gcc -Wall -I../include -L../lib -o $@.tmp $< -lmyc
	mv $@.tmp $@

echo:
	echo $(basename $(allsource))
