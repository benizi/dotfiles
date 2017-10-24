/**
 * Based on: http://datenparkplatz.de/DiesUndDas/memory_overcommit.cc
 * via LKML: https://lkml.org/lkml/2009/5/10/148
 *
 * Modifications:
 * - Use C++ style instead of C.
 * - Parse cli arguments instead of interactive prompting
 * - Don't assume values are MiB (bytes unless trailing 'K', 'M', or 'G')
 */
#include <iostream>
#include <limits>
#include <new>
#include <string>
#include <vector>

namespace {

using namespace std;

#define FAIL(msg) { cout << msg << endl; exit(EXIT_FAILURE); }

// Parse a string size into an amount of memory to allocate.
static size_t parse(const string& size) {
  size_t converted = 0;
  unsigned long val = 0;
  unsigned long shift = 0;
  unsigned long max_ul = numeric_limits<unsigned long>::max();
  size_t max_sz = numeric_limits<size_t>::max();

  // Parse the size to an unsigned int.
  try {
    val = stoul(size, &converted, 0);
  } catch (const invalid_argument& e) {
    FAIL("Invalid input: [" << size << "]");
  } catch (const out_of_range& e) {
    FAIL("Value out of range: [" << size << "]");
  } catch (const exception& e) {
    FAIL("Couldn't parse [" << size << "]: " << e.what() << " failed");
  }

  // Parse out a 'K', 'M', or 'G' suffix.
  string left(size.c_str() + converted);
  if (left == "G") shift = 30;
  else if (left == "M") shift = 20;
  else if (left == "K") shift = 10;
  else if (left != "") FAIL("Unparsed: [" << left << "] in [" << size << "]");

  // Check that the value is within range.
  if (val > max_ul >> shift) FAIL("Too big for " << left << ": " << val);
  if (val == 0) FAIL("Can't alloc 0.");
  val <<= shift;
  if (val > max_sz) FAIL("Too big for size_t: " << val);
  return val;
}

}  // namespace

// Allocate one chunk of memory for each commandline argument.
int main(int argc, char** argv) {
  size_t total = 0;
  vector<size_t> sizes;
  vector<char*> alloced;

  // Parse arguments into sizes
  for (int i=1; i<argc; i++) {
    string arg(argv[i]);
    sizes.push_back(parse(arg));
  }

  // For each size, allocate a char[] of that size.
  for (const size_t& size : sizes) {
    cout << "malloc(" << size << ") ... ";
    fflush(stdout);
    char* mem = nullptr;
    try {
      mem = new char[size]();
    } catch (const exception& e) {
      FAIL("FAILED");
    }
    alloced.push_back(mem);
    cout << "OK" << endl;
    total += size;
    cout << "Running total: " << total << endl;
  }

  // Free everything allocated.
  for (char* mem : alloced) delete[] mem;

  return EXIT_SUCCESS;
}
