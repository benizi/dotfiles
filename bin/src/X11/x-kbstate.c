#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#include <X11/extensions/XKBstr.h>
#include <stdio.h>
#include <string.h>

const char* bitblock[] = {
	" ", /* 0000 - space */
	"▗", /* 0001 - 0x2597 */
	"▖", /* 0010 - 0x2596 */
	"▄", /* 0011 - 0x2584 */
	"▝", /* 0100 - 0x259d */
	"▐", /* 0101 - 0x2590 */
	"▞", /* 0110 - 0x259e */
	"▟", /* 0111 - 0x259f */
	"▘", /* 1000 - 0x2598 */
	"▚", /* 1001 - 0x259a */
	"▌", /* 1010 - 0x258c */
	"▙", /* 1011 - 0x2599 */
	"▀", /* 1100 - 0x2580 */
	"▜", /* 1101 - 0x259c */
	"▛", /* 1110 - 0x259b */
	"█", /* 1111 - 0x2588 */
};

const char *nybbleblock(int b) { return bitblock[b & 0xF]; }
void printbyteblock(unsigned char b) {
	printf("%s", nybbleblock(b >> 4));
	printf("%s", nybbleblock(b));
}

int main(int argc, char **argv) {
	int i, use_unicode = 0, verbose = 0;
	Display *disp = XOpenDisplay(NULL);
	XkbStateRec kbstate;
	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i], "-u")) {
			use_unicode = 1;
		} else if (!strcmp(argv[i], "-v")) {
			verbose = 1;
		}
	}
	if (!disp) return 1;
	if (!XkbGetState(disp, XkbUseCoreKbd, &kbstate)) {
		if (verbose) {
			for (i = 0; i < 7; i++)
				if (((kbstate.mods >> i) & 1))
					printf("Mod%d active\n", i);
		} else if (use_unicode) {
			printbyteblock(kbstate.mods);
		} else
			for (i=7; i>=0; i--)
				printf("%d", ((kbstate.mods >> i) & 1));
		printf("\n");
	} else {
		printf("Request failed.\n");
	}
	XCloseDisplay(disp);
	return 0;
}
