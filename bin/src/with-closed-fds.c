#include <stdio.h>
#include <unistd.h>
#include <linux/limits.h>

int main(int argc, char **argv) {
	int fd = 3;
	if (argc < 2) {
		fprintf(stderr,"Usage: %s program [args]\n",argv[0]);
		return 1;
	}
	while (fd < NR_OPEN) close(fd++);
	execvp(argv[1], argv+1);
	return 1;
}
