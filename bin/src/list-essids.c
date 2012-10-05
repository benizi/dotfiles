#include <stdio.h>
#include <string.h>
#include <iwlib.h>

static int verbose = 0;

static int print_essid(int skfd, char *ifname, char *args[], int count) {
  wireless_config info;
  if (iw_get_basic_config(skfd, ifname, &info))
    return 1;
  if (!info.essid_on && verbose < 2)
    return 0;
  if (verbose)
    printf("%s\t", ifname);
  printf("%s\n", info.essid_on && strlen(info.essid) ? info.essid : "(none)");
  return 0;
}

int main(int argc, char **argv) {
  int i, skfd;
  for (i = 1; i < argc; i++)
    if (!strcmp(argv[i], "-v") || !strcmp(argv[i], "--verbose"))
      verbose++;
  skfd = iw_sockets_open();
  iw_enum_devices(skfd, print_essid, NULL, 0);
  return 0;
}
