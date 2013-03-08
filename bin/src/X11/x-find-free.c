#include <X11/Xlib.h>
#include <stdio.h>

int main(void) {
  int i;
  char name[10];
  Display *display;
  for (i = 0; i < 1000; i++) {
    sprintf(name, ":%d", i);
    if ((display = XOpenDisplay(name))) {
      XCloseDisplay(display);
      continue;
    }
    printf("%s\n", name);
    return 0;
  }
  return 1;
}
