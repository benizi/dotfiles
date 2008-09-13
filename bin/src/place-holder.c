#include <sys/select.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

static void quitter(int sig, siginfo_t *si, void *unused) {
	printf("Quitting from HUP\n");
	exit(0);
}
int main (void) {
	struct timeval t;
	struct sigaction sa;
	sa.sa_flags = SA_SIGINFO;
	sigemptyset(&sa.sa_mask);
	sa.sa_sigaction = quitter;
	if (sigaction(SIGHUP, &sa, NULL) == -1)
		do { perror("sigaction"); exit(1); } while (0);
	while (1) {
		t.tv_sec = 0;
		t.tv_usec = 100;
		select(0,NULL,NULL,NULL,&t);
	}
	return 0;
}
