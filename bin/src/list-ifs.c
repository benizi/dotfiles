#include <stdio.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <netinet/in.h>
#include <string.h>
#include <arpa/inet.h>

#define BUFLEN 1024
#define NAMELEN 32

int main (int argc, char **argv) {
	char buf[BUFLEN];
	struct ifconf ifc;
	struct ifreq *ifr;
	int i, n, sock, just_up = 0, print_addr = 0, just_addr = 0;
	char ifnam[NAMELEN];
	ifnam[0]='\0';

	for (i = 1; i < argc; i++) {
		if (!strcmp(argv[i],"up")) {
			just_up = 1;
		} else if (!strcmp(argv[i],"addr")) {
			print_addr = 1;
		} else if (!strcmp(argv[i],"justaddr")) {
			just_addr = 1;
		} else if (!strcmp(argv[i],"--help")) {
			fprintf(stderr,"Usage: %s [up | addr | justaddr] [ifnames]\n",argv[0]);
		} else {
			strncpy(ifnam, argv[i], NAMELEN);
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
		if (*ifnam && strcmp(item->ifr_name, ifnam)) continue;
		struct sockaddr_in *sin = (struct sockaddr_in *)&item->ifr_addr;
		if (just_up) {
			struct ifreq upreq;
			strncpy(upreq.ifr_name, item->ifr_name, IFNAMSIZ);
			ioctl(sock, SIOCGIFFLAGS, &upreq);
			if (!(upreq.ifr_flags & IFF_UP)) continue;
		}
		if (!just_addr) printf("%s",item->ifr_name);
		if (!just_addr && print_addr) printf("\t");
		if (print_addr||just_addr) printf("%s", inet_ntoa(sin->sin_addr));
		printf("\n");
	}
	return 0;
}
