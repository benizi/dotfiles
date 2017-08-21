#include <unistd.h>
#include <pty.h>

int main(int argc, char **argv) {
  ++argv;
  int pty;
  pid_t iskid = forkpty(&pty, NULL, NULL, NULL);
  if (iskid < 0) return -1;
  if (iskid) execvp(argv[0], argv);
  return 0;
}
