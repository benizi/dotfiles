/* The opposite of nohup, this wraps a program and turns any HUP signal into an
 * INT signal */
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

static int instead = SIGINT;
static pid_t kid;
static void very_noble(int signal) {
	kill(kid, instead);
}

int main (int argc, char **argv) {
	char *arg, *opt;
	int i, status;
	for (i = 1; i < argc; i++) {
		arg = argv[i];
		if (*arg != '-')
			break;
		opt = arg;
		while (*opt == '-')
			opt++;
		if (!strcmp(opt, "kill") || !strcmp(opt, "KILL")) {
			instead = SIGKILL;
		} else {
			fprintf(stderr, "Unknown option: %s\n", arg);
			return 1;
		}
	}
	if (i >= argc) {
		fprintf(stderr, "Usage: %s [--kill] program [args]\n", argv[0]);
		return 1;
	}

	kid = fork();
	if (kid < 0) {
		fprintf(stderr, "Fork failed\n");
		return 1;
	}
	if (kid) {
		signal(SIGHUP, very_noble);
		signal(SIGCHLD, SIG_IGN);
		waitpid(kid, &status, 0);
		return status;
	}
	return execvp(argv[i], argv + i);
}
