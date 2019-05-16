#include <X11/extensions/scrnsaver.h>
#include <stdlib.h>
#include <time.h>

#include <stdio.h>

#define OK(A) { if (!(A)) exit(EXIT_FAILURE); }
#define BREAK(VAR) { VAR = 1; break; }

#define MILLIS 1000
#define NANOS 1000000000
#define DEFAULT_WAIT_MS 1000
#define DEFAULT_TIMEOUT_MS 5000

static long now(void) {
  struct timespec ts;
  OK(!clock_gettime(CLOCK_MONOTONIC, &ts));
  return ts.tv_sec * NANOS + ts.tv_nsec;
}

static long parse_long(const char *s, long def) {
  long l;
  char *err;
  OK(*s);
  l = strtol(s, &err, 0);
  OK(!*err);
  return l ? l : def;
}

int main(int argc, char **argv) {
  long started, wait_idle_ms, timeout_ms;
  int event_base, error_base, timed_out = 0;

  if (argc > 1) wait_idle_ms = parse_long(argv[1], DEFAULT_WAIT_MS);
  if (argc > 2) timeout_ms = parse_long(argv[2], DEFAULT_TIMEOUT_MS);

  started = now();
  Display *disp = XOpenDisplay(NULL);
  OK(disp);
  OK(XScreenSaverQueryExtension(disp, &event_base, &error_base));
  XScreenSaverInfo *info = XScreenSaverAllocInfo();
  OK(info);
  while (1) {
    if (now() >= started + timeout_ms * NANOS / MILLIS) BREAK(timed_out);
    OK(XScreenSaverQueryInfo(disp, DefaultRootWindow(disp), info));
    if (info->idle >= wait_idle_ms) break;
  }
  XFree(info);
  XCloseDisplay(disp);
  exit(timed_out ? EXIT_FAILURE : EXIT_SUCCESS);
}
