#include "libmyc.h"

int main (int argc, char **argv) {
	unsigned int as_int = 0, as_double = 0;
	if (argc > 1) {
		as_int++;
	}
	if (!as_int) as_double++;
	if (as_double) printf("%f\n",NOW());
	if (as_int) printf("%.0f\n",NOW());
	return 0;
}
