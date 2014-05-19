#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int ignore_errors(Display *dpy, XErrorEvent *xev) {
  return 0;
}

int main(int argc, char **argv) {
  Display *dpy;
  int i, id, ignore_unknown;
  char *name;

  if (argc == 1) {
    fprintf(stderr, "Usage: %s atom-id [atom-id]*\n", argv[0]);
    return 1;
  }
  dpy = XOpenDisplay(NULL);
  if (!dpy) {
    fprintf(stderr, "Failed to open display\n");
    return 1;
  }

  XSetErrorHandler(ignore_errors);

  for (i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-i")) {
      ignore_unknown = 1;
      continue;
    }
    id = atoi(argv[i]);
    name = XGetAtomName(dpy, id);
    if (ignore_unknown && !name)
      continue;
    printf("%d\t%s\n", id, name ? name : "(unknown)");
  }

  XCloseDisplay(dpy);
  return 0;
}
