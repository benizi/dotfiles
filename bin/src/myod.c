#define _LARGEFILE64_SOURCE
#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
#define MAX_ROW 16

typedef struct {
	char flag; char *desc;
	int len; int sign; int endy; int test;
	char *form;
	long neglim;
	char *negform;
	int nl;
} outputtype;
static outputtype output[] = {
	{ 'b', "binary",               1, 0, 0, 0, NULL,     0, NULL, 0 },
	{ 'o', "octal bytes",          1, 0, 0, 0, "%03o",   0, NULL, 1 },
	{ 's', "short",                2, 1, 0, 0, "%6d",    0, NULL, 0 },
	{ 'S', "unsigned short",       2, 0, 0, 0, " %5u",   0, NULL, 1 },
	{ 'n', "network short",        2, 0, 1, 0, " %5u",   0, NULL, 0 },
	{ 'N', "network long",         4, 0, 1, 0, " %11u",  0, NULL, 1 },
	{ 'v', "VAX short",            2, 0, 0, 0, " %5u",   0, NULL, 0 },
	{ 'V', "VAX long",             4, 0, 0, 0, " %11u",  0, NULL, 1 },
	{ 'g', "rounded float",        4, 0, 0, 0, "  %.4g", 0, NULL, 0 },
	{ 'G', "net rounded float",    4, 0, 1, 0, "  %.4g", 0, NULL, 1 },
	{ 'd', "double",               8, 0, 0, 0, " %23f",  0, NULL, 0 },
	{ 'D', "net double",           8, 0, 1, 0, " %23f",  0, NULL, 1 },
	{ 'i', "int",                  4, 1, 0, 0, " %11d",  0, NULL, 0 },
	{ 'I', "unsigned int",         4, 0, 0, 0, " %11u",  0, NULL, 0 },
	{ 'l', "long",                 4, 1, 0, 0, " %11d",  0, NULL, 0 },
	{ 'L', "unsigned long",        4, 0, 0, 0, " %11u",  0, NULL, 0 },
	{ '#', "sort-of-signed int",   4, 1, 0, 0, " %11d",  -50000, " %11u", 1 },
	{ 0,   NULL,                   0, 0, 0, 0, NULL,     0, NULL, 0 }
};
typedef struct { char flag; char *longf; char *desc; long *test; } flagtype;
static long blk = 0, dec = 0, space = 0, nopipe = 0, forcepipe = 0, noenv = 0;
static long nochars = 0, notrail = 0, verbose = 0;
static long skinny_row = 0;/*, extra_row = 0;*/
static flagtype flags[] = {
	{ 'C', NULL, "Don't show the ASCII-ish output", &nochars },
	{ 0, NULL, "Display options", NULL },
	{ '_', "space", "show spaces as spaces (default: ·)", &space },
	{ 'T', NULL, "Don't pad the byte-line with trailing '--'s", &notrail },
	{ 'D', "decimal", "show decimal offsets, too", &dec },
	{ 'B', "blocks", "show {512,256,128,64,32}-byte block offsets, too", &blk },
	{ '8', "skinny", "Use rows of 8 bytes, not 16", &skinny_row },
/*	{ '.', "halfrow", "Output a second row offset by half", &extra_row },*/
	{ 0, NULL, "General options", NULL },
	{ 'p', "nopipe", "Don't pipe the result to less", &nopipe },
	{ 'P', "forcepipe", "Force pipe the result to less", &forcepipe },
	{ '+', "verbose", "Verbose output", &verbose },
	{ 'e', "noenv", "Ignore MYOD environment variable", &noenv },
	{ 0, NULL, "Input options", NULL },
	{ 0, NULL, NULL, NULL }
};
static long skip = 0, maxout = 0;
static flagtype intopts[] = {
	{ 'k', "skip", "S_k_ip to N", &skip },
	{ 'x', "max", "Ma_x_ output N", &maxout },
	{ 0, NULL, NULL, NULL }
};
int blksize[] = { 512, 256, 128, 64, 32, 0 };
void usage (char *com) {
	int i;
	printf("Usage: %s [options] [files]\n%s", com, "Output type options\n");
	for (i=0; output[i].len; i++)
		printf("  -%c %s%s", output[i].flag, output[i].desc, output[i].nl?"\n":"");
	for (i=0; flags[i].desc; i++) {
		if (flags[i].flag && flags[i].longf)
			printf("  -%c / --%-9s  %s\n", flags[i].flag, flags[i].longf, flags[i].desc);
		else if (flags[i].flag)
			printf("  -%c %s\n", flags[i].flag, flags[i].desc);
		else
			printf("%s\n", flags[i].desc);
	}
	for (i=0; intopts[i].desc; i++)
		if (intopts[i].longf)
			printf("  -%c / --%-9s N  %s\n", intopts[i].flag, intopts[i].longf, intopts[i].desc);
		else
			printf("  -%c N   %s\n", intopts[i].flag, intopts[i].desc);
	printf("%s",
"Special files\n"
"  -  read from stdin (default if no files given on command line)\n"
);
}

void my_strcpy(char **dst, char *src) {
	*dst = (char *)malloc((1+strlen(src))*sizeof(char));
	strcpy(*dst,src);
}
static pid_t pid;
void passiton (int sig) { kill(pid, sig); }
int main (int argc, char **argv, char **inenv) {
	unsigned char c, signbit, buf[MAX_ROW];
	char *f, *arg, **files, **allargs;
	int pfd[2];
	long off = 0;
	long long buff;
	int i, j, k, r, base, found, longf, fd, inbuf=0;
	int fnum = 0;
	char *env[] = {
		"TERM=xterm",
		"LESS=-R -M --shift 5",
		"LESSBINFMT=*n%c",
		NULL
	};
	if (!strcmp(argv[0],"myodc") || !strcmp(argv[0],"myodcat")) nopipe = 1;
	r = argc;
	for (i = 0; inenv[i]; i++) r++;
	files   = (char **)malloc((r+1)*sizeof(char *));
	allargs = (char **)malloc((r+1)*sizeof(char *));
	if (!files)   { perror("malloc"); return 2; }
	if (!allargs) { perror("malloc"); return 2; }
	for (i = 0; i <= r; i++) { files[i] = NULL; allargs[i] = NULL; }
	for (i = 1, r = 0; argv[i]; i++)
		my_strcpy(&allargs[r++],argv[i]);
	for (i = 0; inenv[i]; i++)
		if (!strncmp(inenv[i],"MYOD=",5))
			my_strcpy(&allargs[r++],inenv[i]+4);
	for (r = 0; *allargs; allargs++) {
		f = *allargs;
		if (!strcmp(f,"-help") || !strcmp(f,"--help")) {
			usage(argv[0]);
			return 0;
		}
		if ((!strncmp(f,"-",1) && strcmp(f,"-"))
			|| (!strncmp(f,"=",1))
			) {
			if (!strncmp(f,"=",1) && noenv) continue;
			longf = (!strncmp(f,"--",2)) ? 1 : 0;
			for (fd = (longf ? 2 : 1); fd < (longf ? 3 : strlen(f)); fd++) {
				for (found = 0, i = 0; output[i].len; i++) {
					if (longf) break;
					if (f[fd] != output[i].flag) continue;
					output[i].test++;
					found++;
					break;
				}
				if (found) continue;
				for (i = 0; flags[i].desc; i++) {
					if (!flags[i].flag) continue;
					if (longf) {
						if (!flags[i].longf) continue;
						if (strcmp(f+fd,flags[i].longf)) continue;
					} else if (f[fd] != flags[i].flag) continue;
					(*flags[i].test)++;
					found++;
					break;
				}
				if (found) continue;
				for (i = 0; intopts[i].desc; i++) {
					if (!intopts[i].flag) continue;
					if (longf) {
						if (!intopts[i].longf) continue;
						if (strcmp(f+fd,intopts[i].longf)) continue;
					} else if (f[fd] != intopts[i].flag) continue;
					arg = allargs[1];
					if (verbose) printf("Processing -%c (%s) with arg <%s>\n", intopts[i].flag, intopts[i].desc, arg);
					if (!arg) {
						for (i = 0; i < 2; i++) {
							printf("** -%c flag (in %s) requires an argument **\n", f[fd], f);
							if (!i) usage(argv[0]);
						}
						return 4;
					}
					base =
						!strncasecmp(arg,"0x",2) ? 16
						: !strncmp(arg,"0",1) ? 8
						: 10;
					j = 0;
					if (base != 10) j++;
					if (base > 10) j++;
					if (verbose) printf("Base=%d\n", base);
					*intopts[i].test = 0;
					for (; arg[j]; j++) {
						*(intopts[i].test) *= base;
						c = arg[j];
						if (c >= 'A' && c <= 'F') { k = 10 + (c - 'A'); }
						else if (c >= 'a' && c <= 'f') { k = 10 + (c - 'a'); }
						else if (c >= '0' && c <= '9') { k = c - '0'; }
						else { k = -1; }
						if (k < 0 || k >= base) {
							for (i = 0; i < 2; i++) {
								printf("** Bad character (%c) in argument to -%c (%s) **\n", c, f[fd], f);
								if (!i) usage(argv[0]);
							}
							return 5;
						}
						*intopts[i].test += k;
					}
					found++;
				}
				if (found && !longf && f[fd+1]) {
					for (i = 0; i < 2; i++) {
						printf("** Cannot add more flags (%s) after flag requiring argument -%c (in %s) **\n", f+fd+1, f[fd], f);
						if (!i) usage(argv[0]);
					}
					return 6;
				}
				if (found) { allargs++; continue; }
				if (f[fd] == '-' && !fd) continue;
				if (allargs[1]) continue;
				for (i = 0; i < 2; i++) {
					printf("** Unknown flag: %c (in %s) **\n", f[fd], f);
					if (!i) usage(argv[0]);
				}
				return 3;
			}
			continue;
		}
		files[r++] = f;
		files[r] = NULL;
	}
	if (!forcepipe && !isatty(1)) nopipe++;
	if (!nopipe) {
		signal(SIGINT,passiton);
		if (pipe(pfd) == -1) { perror("pipe"); return 1; }
		if ((pid = fork()) == -1) { perror("fork"); return 1; }
	}
	if (pid || nopipe) { /*parent*/
		if (!nopipe) {
			close(pfd[0]);
			close(1);
			dup2(pfd[1],1);
		}
		do {
			if (files[fnum]) {
				if (!strcmp(files[fnum],"-")) {
					fd = 0;
				} else {
					fd = open(files[fnum],O_RDONLY);
					if (fd == -1) {
						perror("open");
						fprintf(stderr,"while trying to open: %s\n", files[fnum]);
						if (!nopipe) {
							fflush(stdout);
							close(1);
							close(pfd[1]);
							wait(NULL);
						}
						return 2;
					}
				}
			} else {
				fd = 0;
			}
			if (skip != lseek64(fd, skip, SEEK_SET)) {
				while (off < skip) {
					r = read(fd,&c,1);
					if (r == -1) return 1;
					if (r == 0) break;
					off += r;
				}
			} else {
				off = skip;
			}
			if (off != skip) continue;
			int ROW = skinny_row ? 8 : 16;
			while (1) {
				r = read(fd,buf+inbuf,ROW-inbuf);
				if (r == -1) return 1;
				if (r == 0) if (!inbuf) break;
				inbuf += r;
				if (inbuf < ROW) if (r) continue;
				r = inbuf;
				inbuf = 0;
				printf("%08lx", off);
				for (i = 0; i < r; i++) printf(" %02x", buf[i]);
				for (i = r; i < ROW; i++) if (!notrail) printf(" --");
				if (!nochars) {
					printf("  %c%c",0xc2,0xbb);
					for (i = 0; i < r; i++) {
						c = buf[i];
						if (c >= 0x7F) c = '.';
						switch (c) {
							case '\r':	c = 0xbf; break;
							case '\n':	c = 0xac; break;
							case ' ': c = space ? ' ' : 0xb7; break;
							case 0: c = ' '; break;
							default: break;
						}
						if (c < 0x20) c = '.';
						if (c > 0x7f) printf("%c", 0xc2);
						printf("%c",c);
					}
					printf("%c%c",0xc2,0xab);
				}
				printf("\n");
				if (dec) { printf("%ld\n", off); }
				if (blk) {
					printf("block");
					for (i = 0; blksize[i]; i++) {
						if (i) printf("   ");
						if (0) printf("[%d] ", blksize[i]);
						else printf(" ");
						printf("%ld", off/blksize[i]);
						if (off % blksize[i]) printf("(+%ld)",off % blksize[i]);
						else printf("=");
					}
					printf("\n");
				}
				for (i = 0; output[i].len; i++) {
					if (!output[i].test) continue;
					if (output[i].len > r) continue;
					printf("%c%7s",verbose?output[i].flag:' ',"");
//					printf("Would print %s\n", output[i].desc); continue;
					for (k = 0; k <= r - output[i].len; k += output[i].len) {
						if (output[i].flag == 'b') {
							if (k && !(k % 4)) printf("\n%8s", "");
							printf(" %02x ", buf[k]);
							for (j = 0; j < 8; j++)
								printf("%s",
									((buf[k]>>(8-j-1)) & 1)
									? "1"
									: "0");
							continue;
						}
						buff = 0;
						for (j = 0; j < output[i].len; j++) {
							buff <<= 8;
							if (output[i].endy) buff += buf[k+j];
							else buff += buf[k+(output[i].len - j - 1)];
						}
						if (output[i].sign || output[i].neglim) {
							signbit = (buff >> (j * 8 - 1)) & 0x1;
							if (signbit)
								for (; j<(sizeof(long long)); j++)
									buff += 0xff << j * 8;
						}
//						printf(output[i].form, buff);
//						printf(" %ssignbit", signbit?"+":"!");
						if (output[i].neglim && signbit && buff < output[i].neglim) printf(output[i].negform, buff);
						else printf(output[i].form, buff);
					}
					printf("\n");
				}
				off += r;
				if (maxout && off - skip >= maxout) break;
			}
		} while (files[++fnum]);
		close(fd);
		fflush(stdout);
		close(1);
		if (!nopipe) {
			close(pfd[1]);
			wait(NULL);
		}
	} else { /*child*/
		close(pfd[1]);
		close(0);
		if (dup2(pfd[0],0) == -1) { perror("dup2"); return 3; }
		close(pfd[0]);
		execle("/usr/bin/less", "--", "-", NULL, env);
		printf("Bad exec\n");
		return 5;
	}
	return 0;
}
