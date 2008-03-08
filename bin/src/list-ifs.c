#include <stdio.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
#include <string.h>

#define BUFLEN 1024

int main (int argc, char **argv) {
	char buf[BUFLEN];
	struct ifconf ifc;
	struct ifreq *ifr;
	int i, n, sock, just_up = 0;

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i],"up")) {
			just_up = 1;
		} else {
			fprintf(stderr,"Usage: %s [up]\n",argv[0]);
		}
	}
	
	if ((sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket");
		return 1;
	}

	ifc.ifc_len = BUFLEN;
	ifc.ifc_buf = buf;
	if (ioctl(sock, SIOCGIFCONF, &ifc) < 0) {
		perror("ioctl(SIOCGIFCONF)");
		return 1;
	}

	ifr = ifc.ifc_req;
	n = ifc.ifc_len / sizeof(struct ifreq);
	for (i = 0; i < n; i++) {
		struct ifreq *item = &ifr[i];
		if (just_up) {
			struct ifreq upreq;
			strncpy(upreq.ifr_name, item->ifr_name, IFNAMSIZ);
			ioctl(sock, SIOCGIFFLAGS, &upreq);
			if (!(upreq.ifr_flags & IFF_UP)) continue;
		}
		printf("%s\n",item->ifr_name);
	}
	return 0;
}
