#include <sys/select.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#define USECURSES 1
#if USECURSES
#include <curses.h>
#endif

static void quitter(int sig, siginfo_t *si, void *unused) {
	printf("Quitting from signal %d\n", sig);
	exit(0);
}
int main (void) {
	struct timeval t;
	struct sigaction sa;
#if USECURSES
 	int ch;
 	int st = 0;
	initscr();
 	noecho();
	nonl();
 	cbreak();
	endwin();
#endif
	sa.sa_flags = SA_SIGINFO;
	sigemptyset(&sa.sa_mask);
	sa.sa_sigaction = quitter;
	if (sigaction(SIGHUP, &sa, NULL) == -1
	|| sigaction(SIGQUIT, &sa, NULL) == -1
	|| sigaction(SIGINT, &sa, NULL) == -1
	)
		do { perror("sigaction"); exit(1); } while (0);
	while (1) {
#if USECURSES
 		ch = getch();
 		if (ch != -1) {
 			switch (ch) {
 				case 'q': case 'Q': if (st == 0) st = 1; else st = 0; break;
 				case 'u': case 'U': if (st == 1) st = 2; else st = 0; break;
 				case 'i': case 'I': if (st == 2) st = 3; else st = 0; break;
 				case 't': case 'T': if (st == 3) st = 4; else st = 0; break;
 				case 'e': case 'E': if (st == 0) st = 5; else st = 0; break;
 				case 'x': case 'X': if (st == 5) st = 2; else st = 0; break;
 				default: st = 0; break;
 			}
 			if (st == 4) {
 				printf("Quitting from 'quit' or 'exit'\n");
 				break;
 			}
 		}
#endif
		t.tv_sec = 0;
		t.tv_usec = 100;
		select(0,NULL,NULL,NULL,&t);
	}
	return 0;
}
