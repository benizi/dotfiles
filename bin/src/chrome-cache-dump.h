typedef unsigned int uint32;
typedef int int32;
typedef unsigned long uint64;
typedef long int64;
typedef short int16;

///// {{{1 from disk_format.h
typedef uint32 CacheAddr;

const int kIndexTablesize = 0x10000;
const uint32 kIndexMagic = 0xC103CAC3;
const uint32 kCurrentVersion = 0x20000;  // Version 2.0.

struct LruData {
  int32     pad1[2];
  int32     filled;          // Flag to tell when we filled the cache.
  int32     sizes[5];
  CacheAddr heads[5];
  CacheAddr tails[5];
  CacheAddr transaction;     // In-flight operation target.
  int32     operation;       // Actual in-flight operation.
  int32     operation_list;  // In-flight operation list.
  int32     pad2[7];
};
typedef struct LruData LruData;

// Header for the master index file.
struct IndexHeader {
  /*IndexHeader();*/

  uint32      magic;
  uint32      version;
  int32       num_entries;   // Number of entries currently stored.
  int32       num_bytes;     // Total size of the stored data.
  int32       last_file;     // Last external file created.
  int32       this_id;       // Id for all entries being changed (dirty flag).
  CacheAddr   stats;         // Storage for usage data.
  int32       table_len;     // Actual size of the table (0 == kIndexTablesize).
  int32       crash;         // Signals a previous crash.
  int32       experiment;    // Id of an ongoing test.
  uint64      create_time;   // Creation time for this set of files.
  int32       pad[52];
  LruData     lru;           // Eviction control data.
};
typedef struct IndexHeader IndexHeader;

// The structure of the whole index file.
struct Index {
  IndexHeader header;
  //CacheAddr   table[kIndexTablesize];  // Default size. Actual size controlled
                                       // by header.table_len.
  CacheAddr   *table;  // Default size. Actual size controlled
};

// Main structure for an entry on the backing storage. If the key is longer than
// what can be stored on this structure, it will be extended on consecutive
// blocks (adding 256 bytes each time), up to 4 blocks (1024 - 32 - 1 chars).
// After that point, the whole key will be stored as a data block or external
// file.
struct EntryStore {
  uint32      hash;               // Full hash of the key.
  CacheAddr   next;               // Next entry with the same hash or bucket.
  CacheAddr   rankings_node;      // Rankings node for this entry.
  int32       reuse_count;        // How often is this entry used.
  int32       refetch_count;      // How often is this fetched from the net.
  int32       state;              // Current state.
  uint64      creation_time;
  int32       key_len;
  CacheAddr   long_key;           // Optional address of a long key.
  int32       data_size[4];       // We can store up to 4 data streams for each
  CacheAddr   data_addr[4];       // entry.
  uint32      flags;              // Any combination of EntryFlags.
  int32       pad[4];
  uint32      self_hash;          // The hash of EntryStore up to this point.
  char        key[256 - 24 * 4];  // null terminated
};
typedef struct EntryStore EntryStore;

//COMPILE_ASSERT(sizeof(EntryStore) == 256, bad_EntyStore);
/*const int kMaxInternalKeyLength = 4 * sizeof(EntryStore) -*/
/*                                  offsetof(EntryStore, key) - 1;*/

// Possible states for a given entry.
enum EntryState {
  ENTRY_NORMAL = 0,
  ENTRY_EVICTED,    // The entry was recently evicted from the cache.
  ENTRY_DOOMED      // The entry was doomed.
};

// Flags that can be applied to an entry.
enum EntryFlags {
  PARENT_ENTRY = 1,         // This entry has children (sparse) entries.
  CHILD_ENTRY = 1 << 1      // Child entry that stores sparse data.
};

#pragma pack(push, 4)
// Rankings information for a given entry.
struct RankingsNode {
  uint64      last_used;        // LRU info.
  uint64      last_modified;    // LRU info.
  CacheAddr   next;             // LRU list.
  CacheAddr   prev;             // LRU list.
  CacheAddr   contents;         // Address of the EntryStore.
  int32       dirty;            // The entry is being modifyied.
  uint32      self_hash;        // RankingsNode's hash.
};
typedef struct RankingsNode RankingsNode;
#pragma pack(pop)

//COMPILE_ASSERT(sizeof(RankingsNode) == 36, bad_RankingsNode);

const uint32 kBlockMagic = 0xC104CAC3;
const int kBlockHeaderSize = 8192;  // Two pages: almost 64k entries
const int kMaxBlocks = (8192 - 80) * 8; //const int kMaxBlocks = (kBlockHeaderSize - 80) * 8;

// Bitmap to track used blocks on a block-file.
/*typedef uint32 AllocBitmap[kMaxBlocks / 32];*/

// A block-file is the file used to store information in blocks (could be
// EntryStore blocks, RankingsNode blocks or user-data blocks).
// We store entries that can expand for up to 4 consecutive blocks, and keep
// counters of the number of blocks available for each type of entry. For
// instance, an entry of 3 blocks is an entry of type 3. We also keep track of
// where did we find the last entry of that type (to avoid searching the bitmap
// from the beginning every time).
// This Structure is the header of a block-file:
struct BlockFileHeader {
  /*BlockFileHeader();*/

  uint32          magic;
  uint32          version;
  int16           this_file;    // Index of this file.
  int16           next_file;    // Next file when this one is full.
  int32           entry_size;   // Size of the blocks of this file.
  int32           num_entries;  // Number of stored entries.
  int32           max_entries;  // Current maximum number of entries.
  int32           empty[4];     // Counters of empty entries for each type.
  int32           hints[4];     // Last used position for each entry type.
  volatile int32  updating;     // Keep track of updates to the header.
  int32           user[5];
  //AllocBitmap     allocation_map;
  uint32 allocation_map[((8192 - 80) * 8) / 32];
};
typedef struct BlockFileHeader BlockFileHeader;

//COMPILE_ASSERT(sizeof(BlockFileHeader) == kBlockHeaderSize, bad_header);

// Sparse data support:
// We keep a two level hierarchy to enable sparse data for an entry: the first
// level consists of using separate "child" entries to store ranges of 1 MB,
// and the second level stores blocks of 1 KB inside each child entry.
//
// Whenever we need to access a particular sparse offset, we first locate the
// child entry that stores that offset, so we discard the 20 least significant
// bits of the offset, and end up with the child id. For instance, the child id
// to store the first megabyte is 0, and the child that should store offset
// 0x410000 has an id of 4.
//
// The child entry is stored the same way as any other entry, so it also has a
// name (key). The key includes a signature to be able to identify children
// created for different generations of the same resource. In other words, given
// that a given sparse entry can have a large number of child entries, and the
// resource can be invalidated and replaced with a new version at any time, it
// is important to be sure that a given child actually belongs to certain entry.
//
// The full name of a child entry is composed with a prefix ("Range_"), and two
// hexadecimal 64-bit numbers at the end, separated by semicolons. The first
// number is the signature of the parent key, and the second number is the child
// id as described previously. The signature itself is also stored internally by
// the child and the parent entries. For example, a sparse entry with a key of
// "sparse entry name", and a signature of 0x052AF76, may have a child entry
// named "Range_sparse entry name:052af76:4", which stores data in the range
// 0x400000 to 0x4FFFFF.
//
// Each child entry keeps track of all the 1 KB blocks that have been written
// to the entry, but being a regular entry, it will happily return zeros for any
// read that spans data not written before. The actual sparse data is stored in
// one of the data streams of the child entry (at index 1), while the control
// information is stored in another stream (at index 2), both by parents and
// the children.

// This structure contains the control information for parent and child entries.
// It is stored at offset 0 of the data stream with index 2.
// It is possible to write to a child entry in a way that causes the last block
// to be only partialy filled. In that case, last_block and last_block_len will
// keep track of that block.
struct SparseHeader {
  int64 signature;          // The parent and children signature.
  uint32 magic;             // Structure identifier (equal to kIndexMagic).
  int32 parent_key_len;     // Key length for the parent entry.
  int32 last_block;         // Index of the last written block.
  int32 last_block_len;     // Lenght of the last written block.
  int32 dummy[10];
};
typedef struct SparseHeader SparseHeader;

// The SparseHeader will be followed by a bitmap, as described by this
// structure.
struct SparseData {
  SparseHeader header;
  uint32 bitmap[32];        // Bitmap representation of known children (if this
                            // is a parent entry), or used blocks (for child
                            // entries. The size is fixed for child entries but
                            // not for parents; it can be as small as 4 bytes
                            // and as large as 8 KB.
};

// The number of blocks stored by a child entry.
const int kNumSparseBits = 1024;
//COMPILE_ASSERT(sizeof(SparseData) == sizeof(SparseHeader) + kNumSparseBits / 8,
               /*Invalid_SparseData_bitmap);*/

///// {{{1 from addr.h
const uint32 kInitializedMask    = 0x80000000;
const uint32 kFileTypeMask       = 0x70000000;
const uint32 kFileTypeOffset     = 28;
const uint32 kNumBlocksMask      = 0x03000000;
const uint32 kNumBlocksOffset    = 24;
const uint32 kFileSelectorMask   = 0x00ff0000;
const uint32 kFileSelectorOffset = 16;
const uint32 kStartBlockMask     = 0x0000FFFF;
const uint32 kFileNameMask       = 0x0FFFFFFF;

enum FileType {
  EXTERNAL = 0,
  RANKINGS = 1,
  BLOCK_256,
  BLOCK_1K,
  BLOCK_4K,
};
typedef char FileType;

const int kMaxBlockSize = 4096 * 4;
const int kMaxBlockFile = 255;
const int kMaxNumBlocks = 4;
const int kFirstAdditionalBlockFile = 4;

// Defines a storage address for a cache record
//
// Header:
//   1000 0000 0000 0000 0000 0000 0000 0000 : initialized bit
//   0111 0000 0000 0000 0000 0000 0000 0000 : file type
//
// File type values:
//   0 = separate file on disk
//   1 = rankings block file
//   2 = 256 byte block file
//   3 = 1k byte block file
//   4 = 4k byte block file
//
// If separate file:
//   0000 1111 1111 1111 1111 1111 1111 1111 : file#  0 - 268,435,456 (2^28)
//
// If block file:
//   0000 1100 0000 0000 0000 0000 0000 0000 : reserved bits
//   0000 0011 0000 0000 0000 0000 0000 0000 : number of contiguous blocks 1-4
//   0000 0000 1111 1111 0000 0000 0000 0000 : file selector 0 - 255
//   0000 0000 0000 0000 1111 1111 1111 1111 : block#  0 - 65,535 (2^16)
CacheAddr addr_value(FileType file_type, int max_blocks, int block_file, int index) {
  return ((file_type << kFileTypeOffset) & kFileTypeMask) |
    (((max_blocks - 1) << kNumBlocksOffset) & kNumBlocksMask) |
    ((block_file << kFileSelectorOffset) & kFileSelectorMask) |
    (index  & kStartBlockMask) | kInitializedMask;
}

typedef char bool;
bool is_initialized(CacheAddr value_) {
  return (value_ & kInitializedMask) != 0;
}

bool is_separate_file(CacheAddr value_) {
  return (value_ & kFileTypeMask) == 0;
}

bool is_block_file(CacheAddr value_) {
  return !is_separate_file(value_);
}

FileType file_type(CacheAddr value_) {
  return (value_ & kFileTypeMask) >> kFileTypeOffset;
}

int file_number(CacheAddr value_) {
  if (is_separate_file(value_))
    return value_ & kFileNameMask;
  else
    return ((value_ & kFileSelectorMask) >> kFileSelectorOffset);
}

int block_size_for_file_type(FileType file_type) {
  switch (file_type) {
    case RANKINGS:
      return 36;
    case BLOCK_256:
      return 256;
    case BLOCK_1K:
      return 1024;
    case BLOCK_4K:
      return 4096;
    default:
      return 0;
  }
}

FileType RequiredFileType(int size) {
  if (size < 1024)
    return BLOCK_256;
  else if (size < 4096)
    return BLOCK_1K;
  else if (size <= 4096 * 4)
    return BLOCK_4K;
  else
    return EXTERNAL;
}
