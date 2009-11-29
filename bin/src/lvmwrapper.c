#include <unistd.h>
#include <linux/limits.h>

int main(int argc, char **argv) {
	int fd = 3;
	while (fd < NR_OPEN) close(fd++);
	execvp("lvm", argv);
	return 0;
}
