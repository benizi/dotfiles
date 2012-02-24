#include "libmyc.h"

int main (int argc, char **argv) {
	unsigned int i, newline = 1, as_int = 0, as_double = 0;
	char *arg, *opt, *nl, *yesnl = "\n", *nonl = "";
	for (i=1; i<argc; i++) {
		arg = argv[i];
		if (!starts_with(arg,"-")) die("Unknown argument %s\n",arg);
		opt = arg;
		while (*opt && opt[0] == '-') opt++;
		if (is_in(opt,"int","i",(char*)NULL)) {
			as_int = 1;
		} else if (is_in(opt,"double","d",(char*)NULL)) {
			as_double = 1;
		} else if (is_in(opt,"b","bare","nonl","no-newline",(char*)NULL)) {
			newline = 0;
		} else {
			die("Unknown option %s\n",opt);
		}
	}
	nl = newline ? yesnl : nonl;
	if (!as_int) as_double++;
	if (as_double) printf("%f%s",NOW(),nl);
	if (as_int) printf("%.0f%s",NOW(),nl);
	return 0;
}
