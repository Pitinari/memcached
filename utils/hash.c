#include "hash.h"

unsigned hash_string(char *s) {
  unsigned hashval;
  for (hashval = 0; *s != '\0'; ++s) {
    hashval = *s + 89 * hashval;
  }
  return hashval;
}