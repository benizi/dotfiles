#include <X11/Xlib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define LONGENOUGH 4096
int main(void) {
  Display *disp;
  char buf[LONGENOUGH];
  sprintf(buf, "XCOMPOSEFILE=%s/.XCompose", getenv("HOME"));
  putenv(buf);
  sprintf(buf, "XCOMPOSECACHE=%s/.XCompose.cache", getenv("HOME"));
  putenv(buf);
  disp = XOpenDisplay(NULL);
  if (disp) XCloseDisplay(disp);
}
