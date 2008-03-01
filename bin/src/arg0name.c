#include <unistd.h>
#include <stdio.h>

int usage (char *progname, char *err) {
	fprintf(stderr,"Usage: %s progname [arg0name [args]]\n%s%s",
		progname, err?err:"", err?"\n":"");
	return 1;
}

int main (int argc, char **argv) {
	char *progname;
	if (argc < 2) return usage(argv[0],"Too few arguments");
	argv++;
	if (!strcmp(argv[0],"arg0name")) return usage(argv[0],"Don't call arg0name on itself");
	progname = argv[0];
	if (argc > 2) argv++;
	execvp(progname,argv);
	return 0;
}
