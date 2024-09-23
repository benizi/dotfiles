CFLAGS := -Wall
CXXFLAGS := -Wall
SRC = src
allsource = $(wildcard $(SRC)/*.c $(SRC)/*.go $(SRC)/X11/*.c)

GO_STATIC_FLAGS = -a -tags netgo -ldflags '-w -extldflags -static'
GO_STATIC_EXT = .static
ifeq ($(GO_EXT),$(GO_STATIC_EXT))
	GO_BUILD_FLAGS = $(GO_STATIC_FLAGS)
else
	GO_BUILD_FLAGS =
endif

all:	$(notdir $(basename $(allsource)))

place-holder:	src/place-holder.c
	$(CC) $(CFLAGS) -o $@.tmp $< $(shell pkg-config ncurses --libs)
	mv $@.tmp $@

%:	src/%.c
	$(CC) $(CFLAGS) -o $@.tmp $<
	mv $@.tmp $@

%:	src/X11/%.c
	$(CC) $(CFLAGS) -I$(HOME)/include $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

%$(GO_EXT):	src/%.go
	go list -json $< \
		| jq -r '(.DepsErrors//[])[].ImportStack[-1]' \
		| xargs -r go get -v $(GO_BUILD_FLAGS)
	go build $(GO_BUILD_FLAGS) -o $@ $<

-i:	src/dash-i.go
	go build $(GO_BUILD_FLAGS) -o $@ $<

%$(GO_STATIC_EXT)$(GO_EXT):
	@test x = x$(GO_EXT)
	$(MAKE) GO_EXT=$(GO_STATIC_EXT) $@

%:	src/%.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -o $@.tmp $<
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

x-find-free:	src/X11/x-find-free.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11)
	mv $@.tmp $@

x-idle:	src/X11/x-idle.c
	$(CC) $(CFLAGS) $(shell pkg-config --cflags x11) -o $@.tmp $< $(shell pkg-config --libs x11) $(shell pkg-config --libs xscrnsaver)
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

chrome-cache-dump:	src/chrome-cache-dump.c src/chrome-cache-dump.h
	$(CC) $(CFLAGS) -o $@ src/chrome-cache-dump.c

list-essids:	src/list-essids.c
	$(CC) $(CFLAGS) -o $@ $< -liw -lm

openssl-evp-key:	src/openssl-evp-key.c
	$(CC) $(CFLAGS) -o $@ $< -lcrypto

password-manager:	src/password-manager.c
	$(CC) $(CFLAGS) -Wno-deprecated-declarations $(shell pkg-config --cflags gnome-keyring-1) -o $@ $< $(shell pkg-config --libs gnome-keyring-1)

time-limit:	src/time-limit.c
	$(CC) $(CFLAGS) -I../include -L../lib -o $@.tmp $< -lmyc
	mv $@.tmp $@

fakepty:	src/fakepty.c
	$(CC) $(CFLAGS) -o $@.tmp $< -lutil
	mv $@.tmp $@

echo:
	echo $(basename $(allsource))
