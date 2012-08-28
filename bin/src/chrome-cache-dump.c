#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <utime.h>
#include <fcntl.h>
#include "chrome-cache-dump.h"
#define EPOCH_1600 11644473600000000L

static void date_from_uint64(const uint64 time64, struct timeval *time) {
  uint64 unix_time = time64 - EPOCH_1600;
  time->tv_sec = (unix_time / 1000000);
  time->tv_usec = (unix_time % 1000000);
}

static int block_file[4];

static void try_read(int fd, void *buf, size_t len) {
  ssize_t r = read(fd, buf, len);
  if (r != len) {
    fprintf(stderr, "Read %ld bytes of %ld attempted\n", r, len);
    exit(1);
  }
}

static CacheAddr *index_table(const char *filename, size_t *n_entries) {
  CacheAddr *table;
  int indexfile;
  unsigned long table_len;
  IndexHeader idx;

  indexfile = open(filename, O_RDONLY);
  if (indexfile < 0) {
    fprintf(stderr, "Failed to open index file %s\n", filename);
    exit(1);
  }

  try_read(indexfile, &idx, sizeof(idx));

  if (idx.magic != kIndexMagic) {
    fprintf(stderr, "Index magic number (%d) didn't match expected (%d)\n", idx.magic, idx.magic);
    exit(1);
  }

  if (idx.table_len) {
    *n_entries = idx.table_len;
  } else {
    *n_entries = kIndexTablesize;
  }
  table_len = *n_entries * sizeof(CacheAddr);

  if (!table_len) {
    fprintf(stderr, "Table length is zero?\n");
    exit(1);
  }

  fprintf(stderr, "index num_entries = %d\n", idx.num_entries);
  fprintf(stderr, "index num_bytes = %d\n", idx.num_bytes);
  fprintf(stderr, "index last_file = %d\n", idx.last_file);
  fprintf(stderr, "index table_len = %ld\n", table_len);

  table = (CacheAddr *)malloc(table_len);
  if (!table) {
    fprintf(stderr, "Couldn't malloc(%ld bytes) for index table\n", table_len);
    exit(1);
  }

  try_read(indexfile, table, table_len);

  close(indexfile);

  return table;
}

static void load_block_files() {
  char fname[2048]; // TODO - [PATH_MAX];?
  int i;
  for (i = 0; i < 4; i++) {
    sprintf(fname, "data_%d", i);
    block_file[i] = open(fname, O_RDONLY);
    if (block_file[i] < 0) {
      fprintf(stderr, "Failed to open block file \"%s\"\n", fname);
      exit(1);
    }
  }
}

static void close_block_files() {
  int i;
  for (i = 0; i < 4; i++)
    close(block_file[i]);
}

static int load_cache_block_len(CacheAddr, void *, uint32, uint32);
static int load_cache_block(CacheAddr c, void *buf) {
  return load_cache_block_len(c, buf, 0, block_size_for_file_type(file_type(c)));
}
static int load_cache_block_len(CacheAddr c, void *buf, uint32 off, uint32 len) {
  int fnum, fd, bs, pos;
  if (is_separate_file(c))
    return -1; // TODO
  fnum = file_number(c);
  fd = block_file[fnum];
  bs = block_size_for_file_type(file_type(c));
  pos = ((c & kStartBlockMask) * bs) + kBlockHeaderSize + off;
  lseek(fd, pos, SEEK_SET);
  try_read(fd, buf, len);
  return 0;
}

static int file_complete(const char *filename, uint32 len) {
  struct stat info;
  if (stat(filename, &info))
    return 0;
  return info.st_size == len;
}

static void touch(const char *filename, const uint64 time) {
  struct timeval times[2];
  date_from_uint64(time, &times[0]);
  date_from_uint64(time, &times[1]);
  /*printf("Touch %s @%ld\n", filename, times[0].tv_sec);*/
  utimes(filename, times);
}

static void dump_file(const CacheAddr c, const char *outname, const uint32 len) {
  char buf[4096]; // TODO - magic
  int ofd;
  ofd = open(outname, O_WRONLY|O_CREAT|O_TRUNC, S_IRUSR|S_IWUSR);
  if (ofd < 0) {
    fprintf(stderr, "Could not create %s\n", outname);
    exit(1);
  }

  if (is_separate_file(c)) {
    int fd;
    ssize_t r;
    char inname[4096]; // TODO - magic
    sprintf(inname, "f_%06x", file_number(c));
    fd = open(inname, O_RDONLY);
    if (fd < 0) {
      fprintf(stderr, "Couldn't open %s (for %d [%x])\n", inname, file_number(c), file_number(c));
      exit(1);
    }
    while (1) {
      r = read(fd, buf, sizeof(buf));
      if (!r)
        break;
      if (r < 0) {
        fprintf(stderr, "Couldn't read\n");
        exit(1);
      }
      write(ofd, buf, r);
    }
    close(fd);
  } else {
    int bs = block_size_for_file_type(file_type(c));
    int l = 0;
    uint32 left = len;
    while (left > 0) {
      uint32 to_read = bs > left ? left : bs;
      load_cache_block_len(c, buf, l, to_read);
      l += to_read;
      left -= to_read;
      write(ofd, buf, to_read);
    }
  }
  close(ofd);
  printf("Dumped %s\n", outname);
}

static int dir_exists(const char *dir) {
  struct stat info;
  if (stat(dir, &info))
    return 0;
  return S_ISDIR(info.st_mode);
}

static void mkdir_hier(const char *file) {
  char dir[2048];
  char *last_slash;
  for (last_slash = file; last_slash; last_slash = index(last_slash + 1, '/')) {
    strncpy(dir, file, (last_slash - file));
    dir[last_slash - file] = '\0';
    if (!strlen(dir))
      continue;
    if (dir_exists(dir))
      continue;
    if (mkdir(dir, 0700)) {
      fprintf(stderr, "Failed to mkdir(%s)\n", dir);
      exit(1);
    }
  }
}

static void try_dumping(const char *key, const CacheAddr c, const uint32 len, const uint64 updated) {
  char outname[2048]; // TODO - PATH_MAX
  char *prefixes[] = { "http://", "https://" };
  int start, i, l, o;

  sprintf(outname, "/tmp/cache-test/");
  l = strlen(outname);
  start = 0;
  for (i = 0; i < sizeof(prefixes); i++) {
    if (strncmp(key, prefixes[i], strlen(prefixes[i])))
      continue;
    start = strlen(prefixes[i]);
    break;
  }

  for (i = start, o = l; key[i]; i++, o++) {
    outname[o] = key[i];
  }
  if (outname[o - 1] == '/')
    outname[o - 1] = '_';
  outname[o] = '\0';
  while (dir_exists(outname)) {
    outname[o++] = '_';
    outname[o++] = '\0';
  }

  if (!file_complete(outname, len)) {
    mkdir_hier(outname);
    dump_file(c, outname, len);
  }

  if (file_complete(outname, len))
    touch(outname, updated);
}

int main(int argc, char **argv) {
  CacheAddr *index;
  size_t n_entries;
  char *key;
  int i,j;
  char **patterns;

  patterns = (char **)calloc(argc, sizeof(char *));
  if (!patterns) {
    fprintf(stderr, "Couldn't calloc(%d, %ld) for patterns\n", argc, sizeof(char *));
    exit(1);
  }

  for (i = 1, j = 0; i < argc; i++) {
    if (!strcmp(argv[i], "--dry")) {
      dry_run = 1;
    } else {
      patterns[j] = (char *)calloc(strlen(argv[i]) + 1, sizeof(char));
      strcpy(patterns[j++], argv[i]);
    }
  }
  patterns[j] = NULL;

  index = index_table("index", &n_entries);
  load_block_files();

  CacheAddr inserted = 0;
  for (i = 0; i < n_entries; i++) {
    CacheAddr c;
    EntryStore e;
    if (is_initialized(inserted)) {
      i--;
      c = inserted;
      inserted = 0;
    } else {
      c = index[i];
    }
    if (!is_initialized(c))
      continue;
    //printf("Cache entry %d:\n", i);
    //printf(" Value = 0x%X\n", c);
    //if (is_separate_file(c))
      //printf(" Separate file\n");
    //printf(" File number = %d\n", file_number(c));
    load_cache_block(c, &e);
    if (is_initialized(e.long_key)) {
      //printf(" Long key\n");
      continue; // TODO
    }
    key = e.key;
    if (key[sizeof(e.key)-1] != '\0') {
      key[sizeof(e.key)-1] = '\0';
    }
    // printf(" Key = %s\n", key);
    if (is_initialized(e.next)) {
      // printf(" Next = 0x%08X\n", e.next);
      inserted = e.next;
    }
    //printf(" Created at = %lu\n", e.creation_time);
    for (j = 0; j < 4; j++) {
      if (!is_initialized(e.data_addr[j]))
        continue;
      //printf("  Data stream %d = %d bytes at 0x%08X\n", j, e.data_size[j], e.data_addr[j]);
      if (j != 1)
        continue;
      char **pat;
      int matched = 0;
      for (pat = patterns; *pat; pat++)
        if (strstr(key, *pat))
          matched++;
      if (pat == patterns)
        matched++;
      if (matched)
        try_dumping(key, e.data_addr[j], e.data_size[j], e.creation_time);
    }
  }

  close_block_files();

  return 0;
}
