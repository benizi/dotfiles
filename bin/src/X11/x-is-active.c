#include <X11/Xlib.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>

#define MAX_DISPLAY 1024

void arg_error() {
	fprintf(stderr,"Options: [-display DISPLAY] [-delay N] [-maxtry N | -wait] [-v]\n");
	exit(2);
}

void print_display(char *display_name) {
	if (!display_name) printf("default ");
	printf("display");
	if (display_name) printf("=%s", display_name);
}

static int handler(Display *disp, XErrorEvent *error) {
	fprintf(stderr, "Got an error\n");
	return 0;
}

int main(int argc, char **argv) {
	Display *disp;
	char c, *arg;
	int i, errlen, fds[2], tries = 0;
	fd_set errpipe;
	struct timeval nowait;

	char *display_name = NULL;
	int delay = 1, max_try = 3, verbose = 0;

	for (i = 1; i < argc; i++) {
		arg = argv[i];
		if (!strcmp(arg,"-display")) {
			if (i+1 >= argc) arg_error();
			display_name = (char *)malloc(MAX_DISPLAY * sizeof(char));
			strncpy(display_name, argv[++i], MAX_DISPLAY);
			display_name[MAX_DISPLAY-1] = '\0';
		} else if (!strcmp(arg,"-delay")) {
			if (i+1 >= argc) arg_error();
			delay = atoi(argv[++i]);
		} else if (!strcmp(arg,"-maxtry")) {
			if (i+1 >= argc) arg_error();
			max_try = atoi(argv[++i]);
		} else if (!strcmp(arg,"-wait")) {
			max_try = 0;
		} else if (!strcmp(arg,"-v")) {
			verbose++;
		} else arg_error();
	}

	if (pipe(fds)) {
		perror("pipe");
		exit(1);
	}

	if (dup2(fds[1], 2) < 0) {
		perror("dup2");
		exit(1);
	}

	while (1) {
		tries++;
		disp = XOpenDisplay(display_name);
		if (disp) {
			if (verbose) {
				printf("Opened ");
				print_display(display_name);
				printf("\n");
			}
			XCloseDisplay(disp);
			break;
		}

		/* process any errors received by XOpenDisplay */
		for (errlen = 0;;) {
			FD_ZERO(&errpipe);
			FD_SET(fds[0], &errpipe);
			nowait.tv_sec = 0;
			nowait.tv_usec = 0;
			if (!select(fds[0] + 1, &errpipe, NULL, NULL, &nowait)) {
				break;
			}
			if (!read(fds[0], &c, 1)) {
				break;
			}
			if (verbose) printf("%c", c);
			errlen++;
		}
		if (verbose && errlen) printf("Errors of length %d\n", errlen);

		if (max_try && tries >= max_try) {
			if (verbose) {
				printf("Exceeded -maxtry %d while opening ", max_try);
				print_display(display_name);
				printf("\n");
			}
			exit(1);
		}
		sleep(delay);
	}

	return 0;
}
