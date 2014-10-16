#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <stdio.h>
#include <string.h>

/* TODO: limit? */
#define BUF_SIZE 4096

/* Error return possibilities */
#define RUNNING 0
#define NOT_RUNNING 1
#define OTHER_ERROR 2

static void usage(const char *prog) {
  fprintf(stderr, "Usage: %s NAME\n", prog);
  fprintf(stderr, "  Exit with status:\n");
  fprintf(stderr, "    0 - `xpdf -remote NAME` is running.\n");
  fprintf(stderr, "    1 - `xpdf -remote NAME` is not running.\n");
  fprintf(stderr, "    2 - an error occured while checking.\n");
  fprintf(stderr, "%s --help for this help text.\n", prog);
}

int main(int argc, char **argv) {
  char remote[BUF_SIZE];
  Display *display;
  Atom atom;
  Window win;

  if (argc != 2) {
    usage(argv[0]);
    return OTHER_ERROR;
  }

  if (!strcmp("--help", argv[1])) {
    usage(argv[0]);
    return 0;
  }

  sprintf(remote, "xpdf_%s", argv[1]);

  display = XOpenDisplay(NULL);
  if (!display) return OTHER_ERROR;
  atom = XInternAtom(display, remote, False);
  if (atom == None) return OTHER_ERROR;
  win = XGetSelectionOwner(display, atom);

  return (win == None) ? NOT_RUNNING : RUNNING;
}
