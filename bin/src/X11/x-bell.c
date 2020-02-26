#include <X11/Xlib.h>
#include <X11/XKBlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  int i, pct = 100;
  char *arg;
  Window w = None;
  char xkb_bell = 0;

  for (i = 1; i < argc; i++) {
    arg = argv[i];
    if (!strcmp(arg, "-window")) {
      if (i+1 >= argc) goto argfail;
      w = strtol(argv[++i], NULL, 0);
      printf("w = %ld (0x%08lx)\n", w, w);
    } else if (!strcmp(arg, "-xkb")) {
      xkb_bell = 1;
    } else if (!strcmp(arg, "-x")) {
      xkb_bell = 0;
    } else if (!strcmp(arg, "-pct")) {
      if (i+1 >= argc) goto argfail;
      pct = strtol(argv[++i], NULL, 0);
      printf("pct = %d\n", pct);
    }
  }

  Display *disp = XOpenDisplay(NULL);
  if (xkb_bell) XkbBell(disp, w, pct, (Atom)NULL);
  else XBell(disp, pct);

  return EXIT_SUCCESS;

argfail:
  fprintf(stderr, "Failed to parse arguments\n");
  fflush(stderr);

  return EXIT_FAILURE;
}
