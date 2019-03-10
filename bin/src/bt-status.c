#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/uio.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>

#ifndef __packed
#define __packed __attribute__((packed));
#endif

#define MGMT_EV_CMD_COMPLETE 0x0001
#define MGMT_EV_CMD_STATUS 0x0002

#define MGMT_OP_READ_INFO 0x0004

#define MGMT_MAX_NAME_LENGTH (248 + 1)
#define MGMT_MAX_SHORT_NAME_LENGTH (10 + 1)
#define MAX_REPLY_SIZE 512

#define MGMT_SETTINGS_POWERED (1 << 0)

/* Internal structs from BlueZ src/shared/mgmt.c */

// Typedef `xyz_t` and `xyz_ptr` for `struct xyz` and `struct xyz *`.
#define TYPEDEFS(STRUCT) \
  typedef struct STRUCT STRUCT ## _t; \
  typedef struct STRUCT *STRUCT ## _ptr

// Request that gets queued in `struct mgmt.request_queue`
struct mgmt_request {
  unsigned int id;
  uint16_t opcode;
  uint16_t index;
  void *buf;
  uint16_t len;
  // Remaining fields not used in this program:
  // mgmt_request_func_t callback;
  // mgmt_destroy_func_t destroy;
  // void *user_data;
};
TYPEDEFS(mgmt_request);

// Header for commands as sent on the wire (to HCI control channel)
struct mgmt_hdr {
  uint16_t opcode;
  uint16_t index;
  uint16_t len;
} __packed;
TYPEDEFS(mgmt_hdr);
#define mgmt_hdr_sz sizeof(mgmt_hdr_t)

// Header for successful response for read_info operation
struct mgmt_ev_cmd_complete {
  uint16_t opcode;
  uint8_t status;
  uint8_t data[0];
} __packed;
TYPEDEFS(mgmt_ev_cmd_complete);
#define mgmt_ev_cmd_complete_sz sizeof(mgmt_ev_cmd_complete_t)

// Body for successful response for read_info operation
struct mgmt_rp_read_info {
  bdaddr_t bdaddr;
  uint8_t version;
  uint16_t manufacturer;
  uint32_t supported_settings;
  uint32_t current_settings;
  uint8_t dev_class[3];
  uint8_t name[MGMT_MAX_NAME_LENGTH];
  uint8_t short_name[MGMT_MAX_SHORT_NAME_LENGTH];
} __packed;
TYPEDEFS(mgmt_rp_read_info);
#define mgmt_rp_read_info_sz sizeof(mgmt_rp_read_info_t)

// Send a request using vectorized IO (mirrors BlueZ, but probably overkill)
// Returns 0 on success, 1 on error.
static int send_req(int sock, const mgmt_request_ptr req) {
  struct iovec iov;
  iov.iov_base = req->buf;
  iov.iov_len = req->len;
  ssize_t sent = writev(sock, &iov, 1);
  return sent != req->len;
}

// Each request needs a unique identifier.
static unsigned int req_id = 0;

// Fill the request structure for a request to be sent.
static void init_req(mgmt_request_ptr req, uint16_t opcode, uint16_t index) {
  mgmt_hdr_ptr hdr;

  req->len = mgmt_hdr_sz;
  req->buf = malloc(req->len);

  hdr = (mgmt_hdr_ptr)req->buf;
  hdr->opcode = htobs(opcode);
  hdr->index = htobs(index);
  hdr->len = htobs(0);

  req->opcode = opcode;
  req->index = index;
  req->id = req_id++;
}

// Bind a Bluetooth raw socket to the HCI control channel.
static int bind_raw_hci(void) {
  int sock, flags;

  union {
    struct sockaddr sa;
    struct sockaddr_hci hci;
  } addr;

  flags = SOCK_RAW | SOCK_CLOEXEC | SOCK_NONBLOCK;
  sock = socket(AF_BLUETOOTH, flags, BTPROTO_HCI);

  addr.hci.hci_family = AF_BLUETOOTH;
  addr.hci.hci_dev = HCI_DEV_NONE;
  addr.hci.hci_channel = HCI_CHANNEL_CONTROL;

  bind(sock, &addr.sa, sizeof(addr.hci));

  return sock;
}

// Use `write(2)` (not printf) to print a constant string to stdout
#define print(STR) write(1, STR, sizeof(STR))

// Use `write(2)` (not printf) to print a constant string to stdout w/ newline.
#define echo(STR) print(STR "\n")

// `echo` + exit with error status
#define die(STR) { echo(STR); return 1; }

// Stringify any single `ARG`.
#define STRINGIFY(ARG) #ARG

// Macro `ARG` needs another layer of indirection
#define STRINGIFY_MACRO(ARG) STRINGIFY(ARG)

// Debug macro. `here();` on line 123 prints "HERE 123\n" when hit.
#define here() echo("HERE " STRINGIFY_MACRO(__LINE__))

// Parse commandline arguments.
// Return 0 on success, 1 on error.
static int parse_args(int argc, char **argv, int *quiet, uint16_t *index) {
  int bad = 0;
  char c;

  *quiet = 0;
  *index = 0;
  for (int i=1; i<argc; i++) {
    c = argv[i][0];
    if (!strcmp(argv[i], "-q")) *quiet = 1;
    else if (c >= '0' && c <= '9' && argv[i][1] == '\0') *index = c - '0';
    else bad++;
  }

  return !bad;
}

// Write a single character to stdout.
static void write_char(char c) {
  write(1, &c, 1);
}

// Hex digits
static char hex[] = "0123456789abcdef";

// Write a byte in hex notation to stdout.
static void write_hex_byte(uint8_t b) {
  write_char(hex[(b >> 4) & 0xf]);
  write_char(hex[b & 0xf]);
}

// Main program parses args, reads info for selected index.
// Only arguments are:
// "-q" (for quiet) prints nothing, returns result as exit code.
// "0"-"9" single-digit index for which BT controller to query (default 0).
int main(int argc, char **argv) {
  int sock, quiet, powered;
  ssize_t expected, ret;
  uint16_t index;
  uint8_t buf[MAX_REPLY_SIZE];
  uint32_t current_settings;
  mgmt_request_t req;
  mgmt_hdr_ptr reply_hdr;
  mgmt_ev_cmd_complete_ptr reply_cmd_info;
  mgmt_rp_read_info_ptr reply_info;

  if (!parse_args(argc, argv, &quiet, &index)) die("BAD ARG");

  sock = bind_raw_hci();

  init_req(&req, MGMT_OP_READ_INFO, index);
  send_req(sock, &req);

  ret = read(sock, buf, MAX_REPLY_SIZE);
  expected = mgmt_hdr_sz + mgmt_ev_cmd_complete_sz + mgmt_rp_read_info_sz;
  if (ret < expected) {
    printf("TOO SHORT (got %zd, wanted %zd)\n", ret, expected);
    return 1;
  }

  // Parse the reply header.
  reply_hdr = (mgmt_hdr_ptr)buf;
  if (btohs(reply_hdr->opcode) != MGMT_EV_CMD_COMPLETE) die("BAD REPLY OP");
  if (btohs(reply_hdr->index) != index) die("BAD REPLY INDEX");
  if (btohs(reply_hdr->len) + mgmt_hdr_sz != expected) die("BAD REPLY LENGTH");

  // Parse the reply command status.
  reply_cmd_info = (mgmt_ev_cmd_complete_ptr)(buf + mgmt_hdr_sz);
  if (btohs(reply_cmd_info->opcode) != MGMT_OP_READ_INFO) die("BAD CMD OP");
  if (reply_cmd_info->status != 0) die("BAD CMD STATUS");

  // Parse the actual read_info reply.
  reply_info =
      (mgmt_rp_read_info_ptr)(buf + mgmt_hdr_sz + mgmt_ev_cmd_complete_sz);
  current_settings = le32toh(reply_info->current_settings);
  powered = (current_settings & MGMT_SETTINGS_POWERED) != 0;

  // Just exit with 0 (OK) or 1 (NOK) for `quiet` mode.
  if (quiet) return powered ? 0 : 1;

  // Print true or false for powered/not.
  if (powered) print("true");
  else print("false");

  // Print Bluetooth address tab-separated.
  write_char('\t');

  // BT addr is stored in reverse order from what's normally printed.
  for (int i=5; i>=0; i--) {
    write_hex_byte(reply_info->bdaddr.b[i]);
    write_char(i ? ':' : '\n');
  }

  return 0;
}
