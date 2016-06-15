#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <X11/keysym.h>
#include <X11/extensions/XKBstr.h>
#include <stdio.h>
#include <string.h>

static int ignore_errors(Display *dpy, XErrorEvent *xev) {
  return 0;
}

int main(int argc, char **argv) {
  int ret = 0;
  Display *disp = XOpenDisplay(NULL);
  if (!disp) return 1;
  XSetErrorHandler(ignore_errors);
  XkbDescPtr desc = XkbAllocKeyboard();
  if (desc) {
    XkbGetControls(disp, XkbAllControlsMask, desc);
    XkbGetNames(disp, XkbSymbolsNameMask, desc);
    if (desc->names) {
      char *symbols = XGetAtomName(disp, desc->names->symbols);
      if (symbols) {
        if (argc == 1) {
          printf("%s\n", symbols);
        } else {
          ret = (strstr(symbols, argv[1]) != NULL) ? 0 : 1;
        }
      }
    }
  }
  XCloseDisplay(disp);
  return ret;
}
