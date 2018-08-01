#include <linux/nbd.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int main(void) {
  int nbd = open("/dev/nbd0", O_RDWR);
  if (nbd < 0) { perror("open"); return 1; }
  if (ioctl(nbd, NBD_CLEAR_QUE) < 0) perror("ioctl");
  if (ioctl(nbd, NBD_CLEAR_SOCK) < 0) perror("ioctl");
  if (ioctl(nbd, NBD_DISCONNECT) < 0) perror("ioctl");
  return 0;
}
