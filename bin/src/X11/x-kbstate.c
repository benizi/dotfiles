#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#include <X11/extensions/XKBstr.h>
#include <stdio.h>

int main(void) {
	Display *disp = XOpenDisplay(NULL);
	XkbStateRec kbstate;
	int i;
	if (!disp) return 1;
	if (!XkbGetState(disp, XkbUseCoreKbd, &kbstate)) {
		for (i=7; i>=0; i--)
			printf("%d", ((kbstate.mods >> i) & 1));
		printf("\n");
	} else {
		printf("Request failed.\n");
	}
	XCloseDisplay(disp);
	return 0;
}
