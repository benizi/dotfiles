#include <gnome-keyring.h>
#define OK GNOME_KEYRING_RESULT_OK

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>

/* for password prompt */
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static char *
default_user(void)
{
  int i;
  char *vars[] = { "USER", "LOGNAME", NULL };
  char *ret = NULL;
  for (i = 0; vars[i] && !ret; i++)
    ret = getenv(vars[i]);
  return ret;
}

#define FIND_NAMED_KIND(ATT, NAME, KIND) \
  if (!strcmp(att.name, NAME)) { \
    ATT = att.value.KIND; \
    ATT ## _p = 1; \
    continue; \
  }
#define FIND_STRING(ATT) FIND_NAMED_KIND(ATT, #ATT, string)
#define FIND_INT(ATT) FIND_NAMED_KIND(ATT, #ATT, integer)

#define MAX_ATTR_STR 65536
#define MAX_ATTR_NAME 512
#define ATTR_OUTPUT_PADDING 16 /* other characters per line of extra attr */
#define MAX_ATTR_VAL (MAX_ATTR_STR - MAX_ATTR_NAME - ATTR_OUTPUT_PADDING)
#define CHECK(VAR) if (VAR == NULL) { perror("malloc"); return; }

static void
short_key_listing(gchar *keyring, guint32 id)
{
  GnomeKeyringAttributeList *attlist;
  GnomeKeyringResult result =
    gnome_keyring_item_get_attributes_sync(keyring, id, &attlist);

  char *user, *server, *domain, *protocol, *schema;
  int user_p = 0, server_p = 0, domain_p = 0, protocol_p = 0, schema_p = 0;
  char *name, *magic;
  int name_p = 0, magic_p = 0;
  int port;
  int port_p = 0;

  if (result == GNOME_KEYRING_RESULT_OK) {
    GnomeKeyringAttribute att, *atts;
    int i, j, printable, n_extra, len;
    atts = (GnomeKeyringAttribute *)attlist->data;
    char **extra_attrs = (char **)malloc(attlist->len * sizeof(char*));
    CHECK(extra_attrs);
    for (i = 0, n_extra = 0; i < attlist->len; i++) {
      att = atts[i];
      if (att.type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
        FIND_NAMED_KIND(schema, "xdg:schema", string);
        FIND_STRING(user);
        FIND_STRING(server);
        FIND_STRING(domain);
        FIND_STRING(protocol);
        FIND_STRING(name);
        FIND_STRING(magic);
        extra_attrs[n_extra] = (char *)malloc(MAX_ATTR_STR * sizeof(char));
        CHECK(extra_attrs[n_extra]);
        len = strlen(att.value.string);
        if (len > MAX_ATTR_VAL)
          printable = 0;
        else {
          for (j = 0, printable = 1; j < len; j++) {
            if (att.value.string[j] >= 0x20 && att.value.string[j] <= 0x7e)
              continue;
            printable = 0;
            break;
          }
        }
        sprintf(extra_attrs[n_extra++],
            "[%s]=[%s]",
            strlen(att.name) < MAX_ATTR_NAME ? att.name : "(long name)",
            printable ? att.value.string : "(unprintable)");
      } else if (att.type == GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32) {
        FIND_INT(port);
        printf("UNHANDLED INT: %s\n", att.name);
      }
    }
    printf(" ");
    if (user_p && strcmp(user, default_user())) printf("%s@", user);
    if (server_p) printf("%s", server);
    if (domain_p) printf(".%s", domain);
    if (port_p) printf(":%d", port);
    if (protocol_p) printf(" [%s]", protocol);
    if (schema_p && 0) printf(" {%s}", schema);
    if (name_p) printf(" name=%s", name);
    if (magic_p) printf(" magic=%s", magic);
    for (i = 0; i < n_extra; i++) printf("  %s\n", extra_attrs[i]);
    gnome_keyring_attribute_list_free(attlist);
  }
}

static void
detailed_key_listing(gchar *keyring, guint32 id)
{
  GnomeKeyringAttributeList *attlist;
  GnomeKeyringResult result =
    gnome_keyring_item_get_attributes_sync(keyring, id, &attlist);
  if (result == GNOME_KEYRING_RESULT_OK) {
    GnomeKeyringAttribute att, *atts;
    int i;
    atts = (GnomeKeyringAttribute *)attlist->data;
    for (i = 0; i < attlist->len; i++) {
      att = atts[i];
      if (att.type == GNOME_KEYRING_ATTRIBUTE_TYPE_STRING) {
        printf("  %s = '%s'\n", att.name, att.value.string);
      } else if (att.type == GNOME_KEYRING_ATTRIBUTE_TYPE_UINT32) {
        printf("  %s = %u\n", att.name, att.value.integer);
      } else {
        printf("Found %d\n", i);
      }
    }
    gnome_keyring_attribute_list_free(attlist);
  }
}

static void
print_secret(GnomeKeyringItemInfo *info)
{
  char *pass = gnome_keyring_item_info_get_secret(info);
  printf(" SECRET[%ld]=\"", strlen(pass));
  for (; *pass; pass++) {
    switch (*pass) {
      case '\n':
        printf("\\n");
        break;
      case '\t':
        printf("\\t");
        break;
      case '"':
        printf("\\\"");
        break;
      default:
        if (*pass < 0x20 || *pass > 0x7f) {
          printf("\\x%02x", *pass);
        } else {
          printf("%c", *pass);
        }
    }
  }
  printf("\"");
}

static int
key_listing(int verbose, int secret)
{
  GList *keyrings, *current;
  if (gnome_keyring_list_keyring_names_sync(&keyrings) != OK) {
    fprintf(stderr, "Couldn't get list of keyring names\n");
    return 1;
  }

  for (current = keyrings; current; current = current->next) {
    GList *ids, *cid;
    gchar *keyring = (gchar *)current->data;
    printf("Keyring %s\n", keyring);

    if (gnome_keyring_list_item_ids_sync(keyring, &ids) != OK) {
      fprintf(stderr, "Couldn't list IDs on keyring %s\n", keyring);
      continue;
    }

    for (cid = ids; cid; cid = cid->next) {
      GnomeKeyringItemInfo *info;
      GnomeKeyringResult result =
        gnome_keyring_item_get_info_full_sync(keyring, GPOINTER_TO_INT(cid->data), GNOME_KEYRING_ITEM_INFO_SECRET, &info);
      switch (result) {
        case GNOME_KEYRING_RESULT_OK:
          short_key_listing(keyring, GPOINTER_TO_INT(cid->data));
          if (verbose)
            detailed_key_listing(keyring, GPOINTER_TO_INT(cid->data));
          if (secret) {
            if (verbose) printf(" ");
            print_secret(info);
          }
          printf("\n");

          gnome_keyring_item_info_free(info);
          break;
#define CHECKIT(X) case GNOME_KEYRING_RESULT_ ## X: printf(#X "\n"); break
        CHECKIT(DENIED);
        CHECKIT(NO_KEYRING_DAEMON);
        CHECKIT(ALREADY_UNLOCKED);
        CHECKIT(NO_SUCH_KEYRING);
        CHECKIT(BAD_ARGUMENTS);
        CHECKIT(IO_ERROR);
        CHECKIT(CANCELLED);
        CHECKIT(KEYRING_ALREADY_EXISTS);
        CHECKIT(NO_MATCH);
      }
    }
  }

  return 0;
}

#define MAXPASS 65536
static void
password_cleaner(char *password)
{
  int i;
  for (i = 0; password[i] && i < MAXPASS; i++)
    if (password[i] == '\r' || password[i] == '\n')
      break;
  password[i] = '\0';
}

static char *
password_prompt(const char *user, const char *server, const char *domain, const char *protocol, guint32 port)
{
  int status;
  int pid;
  int fd[2];
  char *password;
  password = (char *)malloc(MAXPASS * sizeof(char));
  if (!password) return password;
  password[0] = '\0';

  pipe(fd);

  pid = fork();
  if (pid < 0) return NULL;

  if (pid) { /* parent */
    int ret;
    close(fd[1]);
    ret = read(fd[0], password, MAXPASS);
    close(fd[0]);
    waitpid(-1, &status, 0);
    if (ret < 0 || !*password) password[0] = '\0';
    password_cleaner(password);
  } else {
    char *argv[] = { "picker", "--secret", "-p", "password", NULL };
    close(fd[0]);
    dup2(fd[1], 1);
    close(0);
    execvp("picker", argv);
    exit(1);
  }
  return password;
}

static char *
password_from(int fd)
{
  char *password;
  int ret;

  password = (char *)malloc(MAXPASS * sizeof(char));
  if (!password) return password;
  ret = read(fd, password, MAXPASS);
  if (ret < 0) password[0] = '\0';
  password_cleaner(password);
  return password;
}

static void
set_port_by_protocol(guint32 *port, const char *protocol)
{
  struct servent *srv;
  if (!strcmp("rdp", protocol)) {
    *port = 3389;
    return;
  }
  srv = getservbyname(protocol, NULL);
  if (srv) *port = ntohs(srv->s_port);
}

static void
set_protocol_by_port(char **protocol, int port)
{
  struct servent *srv;
  srv = getservbyport(htons(port), NULL);
  if (srv) *protocol = srv->s_name;
}

static void usage() {
  printf("Usage: password-manager [options]\n");
  printf("\n");
  printf("Password attributes:\n");
  printf("  -u/--user USERNAME\n");
  printf("  -h/--host HOSTNAME | -s/--server SERVER | -d/--domain DOMAIN\n");
  printf("     HOST = SERVER.DOMAIN\n");
  printf("  -P/--protocol PROTOCOL | -p/--port PORTNUMBER | -N/--no-port\n");
  printf("     PROTOCOL = numeric PORT\n");
  printf("  --passfd N = specify a password via file descriptor\n");
  printf("\n");
  printf("  -k/--keyring KEYRING\n");
  printf("\n");
  printf("Run-mode options:\n");
  printf("  -l/--list\n");
  printf("  -R/--remove\n");
  printf("  -n/--dry\n");
  printf("\n");
  printf("Output options:\n");
  printf("  -v/--verbose\n");
}

#define ARG(X) (!strcmp(argv[i], "-" #X))
#define LARG(X) (!strcmp(argv[i], "--" #X))
#define CHECK_ARGS if (i + 1 == argc) { fprintf(stderr, "Missing a required argument\n"); return 1; }
#define S_OPT(X) CHECK_ARGS X = argv[++i]
#define S_OPT_Q(X) S_OPT(X); set_ ## X = 1
#define I_OPT(X) CHECK_ARGS X = strtol(argv[++i], NULL, 0)
#define I_OPT_Q(X) I_OPT(X); set_ ## X = 1

#define MODE(X) { \
  if (remove || list) { \
    fprintf(stderr, "Multiple modes set\n"); \
    return 1; \
  } \
  X = 1; \
}

int main(int argc, char **argv) {
  GnomeKeyringResult result;
  GList *results;
  char *host;
  int i, tried, verbose = 0, secret = 0, remove = 0, list = 0;
  int set_user = 0;
  int set_domain = 0, set_host = 0, set_server = 0;
  int set_protocol = 0, set_port = 0, set_passfd = 0;
  int no_port = 0;
  int dry = 0;

  char *user = default_user();
  char *domain = NULL;
  char *server = NULL;
  char *object = NULL;
  char *protocol = NULL;
  char *authtype = NULL;
  guint32 port = 0;
  int passfd = 0;

  char *use_keyring = GNOME_KEYRING_DEFAULT;

  for (i = 1; i < argc; i++) {
    if (ARG(u) || LARG(user)) {
      S_OPT(user);
    } else if (ARG(h) || LARG(host)) {
      S_OPT_Q(host);
    } else if (ARG(s) || LARG(server)) {
      S_OPT_Q(server);
    } else if (ARG(d) || LARG(domain)) {
      S_OPT_Q(domain);
    } else if (ARG(P) || LARG(protocol)) {
      S_OPT_Q(protocol);
    } else if (ARG(p) || LARG(port)) {
      I_OPT_Q(port);
    } else if (LARG(passfd)) {
      I_OPT_Q(passfd);
    } else if (ARG(N) || LARG(no-port)) {
      no_port = 1;
    } else if (ARG(v) || LARG(verbose)) {
      verbose = 1;
    } else if (ARG(S) || LARG(secret)) {
      secret = 1;
    } else if (ARG(R) || LARG(remove)) {
      MODE(remove);
    } else if (ARG(l) || LARG(list)) {
      MODE(list);
    } else if (ARG(n) || LARG(dry)) {
      dry = 1;
      verbose++;
    } else if (ARG(k) || LARG(keyring)) {
      S_OPT(use_keyring);
    } else if (LARG(help)) {
      usage();
      return 0;
    } else {
      break;
    }
  }

  while (i < argc) {
    char *arg, *found;
    arg = argv[i++];
    while (*arg) {
      if (*arg == '[' && (found = index(arg, ']')) && !strcmp(found, "]")) {
        if (!set_port) {
          arg++;
          protocol = arg;
          set_protocol = 1;
          no_port = 0;
          *found = '\0';
        }
        break;
      } else if (!set_user && (found = index(arg, '@'))) {
        user = arg;
        set_user = 1;
        *found = '\0';
        arg = found + 1;
      } else if (!(set_host || set_server || set_domain)) {
        if ((found = index(arg, ':'))) {
          host = arg;
          set_host = 1;
          *found = '\0';
          port = strtol(found + 1, NULL, 0);
          set_port = 1;
        } else {
          host = arg;
          set_host = 1;
          no_port = 1;
        }
        break;
      } else {
        printf("Unknown argument: %s\n", arg);
        return 1;
      }
    }
  }

  if (set_host && !(set_domain && set_server)) {
    char *dot = index(host, '.');
    if (dot) {
      dot[0] = '\0';
      dot++;
      if (!set_domain) domain = dot;
      if (!set_server) server = host;
    } else {
      server = host;
      domain = "";
    }
  } else if (set_domain && set_server) {
    host = NULL;
  } else if (!list) {
    fprintf(stderr, "Must set --host or (--server and --domain)\n");
    return 1;
  }

  if (!no_port) {
    if ((set_port + set_protocol) == 1) {
      if (set_protocol) {
        set_port_by_protocol(&port, protocol);
      } else if (set_port) {
        set_protocol_by_port(&protocol, port);
      }
    } else if (!(set_port || set_protocol || list)) {
      fprintf(stderr, "Must set exactly one of --port, --protocol, and --no-port\n");
      return 1;
    }

    if (set_protocol && !port) {
      fprintf(stderr, "Couldn't determine port for --protocol %s\n", protocol);
      return 1;
    }
  }

  if (verbose && !list) {
#define VALUE(X) printf("%s: %s\n", #X, X ? X : "(null)")
    VALUE(user);
    VALUE(domain);
    VALUE(server);
    VALUE(host);
    VALUE(protocol);
#undef VALUE
    printf("port: %d\n", port);
    if (dry)
      return 0;
  }

  if (!gnome_keyring_is_available()) {
    fprintf(stderr, "No keyring available\n");
    return 1;
  }

  if (list)
    return key_listing(verbose, secret);

  for (tried = 0; tried < 2; tried++) {
    result = gnome_keyring_find_network_password_sync(
      user, domain, server, object, protocol, authtype, port,
      &results
    );
    if (verbose) printf("attempt #%d: ", tried);
    if (result == OK) {
      GList *current;
      GnomeKeyringNetworkPasswordData *passdata;
      char *password;
      for (i = 0, current = results; current; i++, current = current->next) {
        passdata = (GnomeKeyringNetworkPasswordData *)current->data;
        password = passdata->password;
        if (verbose) {
          printf("Result[%d]=%s\n", i, password);
          continue;
        }
        if (remove) {
          result = gnome_keyring_item_delete_sync(
            passdata->keyring,
            passdata->item_id
          );
          if (verbose)
            printf("Remove %s %d -> %s\n", passdata->keyring, passdata->item_id, result == OK ? "OK" : "NOT OK");
          if (!current->next)
            return 0;
          continue;
        }
        printf("%s", password);
        return 0;
      }
      if (password)
        break;
    }
    if (remove) {
      printf("No such password\n");
      return 1;
    }
    if (verbose) printf("nope\n");
    if (!tried) {
      char *password;
      if (set_passfd) {
        password = password_from(passfd);
      } else {
        password = password_prompt(user, server, domain, protocol, port);
      }
      if (password) {
        guint32 item_id;
        gnome_keyring_set_network_password_sync(
          use_keyring,
          user, domain, server, object, protocol, authtype, port,
          password, &item_id
        );
        if (verbose) printf("Stored password? %s\n", item_id ? "yes" : "no");
      }
    }
  }
  return 0;
}
