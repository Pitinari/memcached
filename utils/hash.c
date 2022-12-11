#include "hash.h"

unsigned hash_function(char *s, unsigned len) {
  unsigned hashval = 0;
  for (unsigned i = 0; i < len; i++) {
    hashval = s[i] + 89 * hashval;
  }
  return hashval;
}