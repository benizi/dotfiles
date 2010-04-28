#include "libmyc.h"
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

#define EXIT_SIGNALED 109
#define EXIT_THISPROG 110
#define EXIT_EXECVP 3

static unsigned int numeric_status = 0;
static int my_exit (int ret) {
	if (verbose) {
		if (numeric_status) {
			fprintf(stderr,"Exit=%d\n",ret);
		} else {
			fprintf(stderr,"Exit=");
			switch (ret) {
				case EXIT_SIGNALED:
					fprintf(stderr,"child signaled");
					break;
				case EXIT_THISPROG:
					fprintf(stderr,"something wrong in this prog");
					break;
				case EXIT_EXECVP:
					fprintf(stderr,"couldn't execvp child");
					break;
				default:
					fprintf(stderr,"code=%d",ret);
					break;
			}
			fprintf(stderr," (@ %f)",NOW());
			fprintf(stderr,"\n");
		}
	}
	return ret;
}

static char *usage = "Usage: %s [options] command [arguments]\n  -t N    wait N seconds\n";
int main (int argc, char **argv, char **env) {
	pid_t pid, err = 0;
	int i, status = 0;
	char f;
	double diff;
	struct timeval start, now;
	unsigned int c = 0, nonpass = 1, wrap = 0, wait = 1;
	if (argc < 2) {
		fprintf(stderr, usage, argv[0]);
		return 1;
	}
	for (i = 1; i < argc; i++) {
		if (argv[i][0] != '-') break;
		nonpass++;
		f = argv[i][1];
		if (f == '-') f = argv[i][2];
		if (verbose>1) fprintf(stderr,"Got flag %c\n",f);
		switch (f) {
			case 'v':
				verbose++;
				break;
			case 's':
				if (verbose>1) fprintf(stderr,"Numeric status\n");
				numeric_status++;
				break;
			case 't':
				if (i+1 >= argc) {
					fprintf(stderr,usage,argv[0]);
					return 2;
				}
				wait = atol(argv[++i]);
				if (verbose>1) fprintf(stderr,"Wait=%d\n",wait);
				nonpass++;
				break;
			default:
				do {
					fprintf(stderr,"Unknown argument: %s\n",argv[i]);
					if (wrap++) return 3;
					fprintf(stderr,usage,argv[0]);
				} while (wrap++);
		}
	}
	if (nonpass >= argc-1) {
		fprintf(stderr,usage,argv[0]);
		return 4;
	}
	if (gettimeofday(&start,NULL)) {
		fprintf(stderr,"Couldn't get time of day\n");
		return 5;
	}
	pid = fork();
	if (pid == -1) {
		fprintf(stderr, "Couldn't fork\n");
		return 2;
	}
	if (verbose>1) fprintf(stderr,"Start:%f\n",time_to_double(start));
	if (pid) {
		while (1) {
			if (gettimeofday(&now,NULL)) {
				fprintf(stderr,"Couldn't get time of day\n");
				return my_exit(EXIT_THISPROG);
			}
			err = waitpid(pid,&status,WNOHANG);
			if (err && err != pid) {
				fprintf(stderr, "Waitpid error\n");
				return my_exit(EXIT_THISPROG);
			} else if (err == pid) {
				if (WIFEXITED(status)) {
					if (verbose>1) fprintf(stderr,"Child exited normally\n");
					err = WEXITSTATUS(status);
				} else if (WIFSIGNALED(status)) {
					if (verbose>1) fprintf(stderr,"Child was signalled\n");
					err = EXIT_SIGNALED;
				}
				if (verbose>1) fprintf(stderr,"Returning %d (status=%d)\n",err,status);
				return my_exit(err);
			} else if (c) {
				if (verbose>1&&c++<2) fprintf(stderr,"Killed but not dead\n");
			} else {
				diff = time_to_double(now) - time_to_double(start);
				if (diff >= (double)wait && !c++) {
					if (verbose>1) fprintf(stderr,"now:%f\n",time_to_double(now));
					if (verbose>1) fprintf(stderr,"now:%ld.%06ld\n",now.tv_sec,now.tv_usec);
					if (verbose>1) fprintf(stderr,"Time limit reached (diff=%f)\n",diff);
					kill(pid,9);
				} else {
					if (verbose>1) fprintf(stderr,"Diff: %f\n",diff);
				}
			}
		}
	} else {
		argv+=nonpass;
		if (verbose>1) {
			fprintf(stderr,"About to execvp\n");
			for (i = 0; i < argc-nonpass; i++)
				fprintf(stderr,"argv[%d]=<%s>\n",i,argv[i]);
		}
		execvp(argv[0], argv);
		fprintf(stderr, "Couldn't execvp\n");
		return my_exit(EXIT_EXECVP);
	}
}
