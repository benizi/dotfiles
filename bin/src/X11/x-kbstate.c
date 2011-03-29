#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#include <X11/extensions/XKBstr.h>
#include <stdio.h>
#include "bitblock.h"

int main(int argc, char **argv) {
	int i, use_unicode = 0;
	Display *disp = XOpenDisplay(NULL);
	XkbStateRec kbstate;
	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-u")) {
			use_unicode = 1;
		}
	}
	if (!disp) return 1;
	if (!XkbGetState(disp, XkbUseCoreKbd, &kbstate)) {
		if (use_unicode) printf("│");
		if (use_unicode) {
			printbyteblock(kbstate.mods);
		} else
			for (i=7; i>=0; i--)
				printf("%d", ((kbstate.mods >> i) & 1));
		if (use_unicode) printf("│");
		printf("\n");
	} else {
		printf("Request failed.\n");
	}
	XCloseDisplay(disp);
	return 0;
}
