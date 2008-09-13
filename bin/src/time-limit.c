#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

static char *usage = "Usage: %s [options] command [arguments]\n  -t N    wait N seconds\n";
int main (int argc, char **argv, char **env) {
	pid_t pid, err = 0;
	int i, status = 0;
	char f;
	double diff;
	struct timeval start, now;
	unsigned int c = 0, verbose = 0, nonpass = 1, wrap = 0, wait = 1;
	if (argc < 2) {
		fprintf(stderr, usage, argv[0]);
		return 1;
	}
	for (i = 1; i < argc; i++) {
		if (argv[i][0] != '-') break;
		nonpass++;
		f = argv[i][1];
		if (f == '-') f = argv[i][2];
		if (verbose) fprintf(stderr,"Got flag %c\n",f);
		switch (f) {
			case 'v':
				verbose++;
				break;
			case 't':
				if (i+1 >= argc) {
					fprintf(stderr,usage,argv[0]);
					return 2;
				}
				wait = atol(argv[++i]);
				if (verbose) fprintf(stderr,"Wait=%d\n",wait);
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
	if (verbose) fprintf(stderr,"Start:%f\n",start.tv_sec+((double)start.tv_usec/100000));
	if (pid) {
		while (1) {
			if (gettimeofday(&now,NULL)) {
				fprintf(stderr,"Couldn't get time of day\n");
				return 110;
			}
			err = waitpid(pid,&status,WNOHANG);
			if (err && err != pid) {
				fprintf(stderr, "Waitpid error\n");
				return 110;
			} else if (err == pid) {
				if (WIFEXITED(status)) {
					if (verbose) fprintf(stderr,"Child exited normally\n");
					err = WEXITSTATUS(status);
				} else if (WIFSIGNALED(status)) {
					if (verbose) fprintf(stderr,"Child was signalled\n");
					err = 109;
				}
				if (verbose) fprintf(stderr,"Returning %d (status=%d)\n",err,status);
				return err;
			} else if (c) {
				if (verbose>1&&c++<2) fprintf(stderr,"Killed but not dead\n");
			} else {
				diff =
					((double)now.tv_sec     + ((double)now.tv_usec  )/1000000)
					- ((double)start.tv_sec + ((double)start.tv_usec)/1000000);
				if (diff >= (double)wait && !c++) {
					if (verbose>1) fprintf(stderr,"now:%f\n",now.tv_sec+((double)now.tv_usec/100000));
					if (verbose>1) fprintf(stderr,"now:%ld.%06ld\n",now.tv_sec,now.tv_usec);
					if (verbose) fprintf(stderr,"Time limit reached (diff=%f)\n",diff);
					kill(pid,9);
				} else {
					if (verbose>1) fprintf(stderr,"Diff: %f\n",diff);
				}
			}
		}
	} else {
		argv+=nonpass;
		if (verbose) {
			fprintf(stderr,"About to execvp\n");
			for (i = 0; i < argc-nonpass; i++)
				fprintf(stderr,"argv[%d]=<%s>\n",i,argv[i]);
		}
		execvp(argv[0], argv);
		fprintf(stderr, "Couldn't execvp\n");
		return 3;
	}
}
