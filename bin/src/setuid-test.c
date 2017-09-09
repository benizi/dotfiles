#define _GNU_SOURCE
#include <stdio.h>
#include <unistd.h>

int main(void) {
  uid_t r, e, s;
  getresuid(&r, &e, &s);
  printf("r=%d e=%d s=%d\n", r, e, s);
  return 0;
}
