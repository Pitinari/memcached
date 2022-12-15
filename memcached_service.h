#ifndef __MEMCACHED_H__
#define __MEMCACHED_H__

#include "./structures/hash_table_with_lru.h"

struct _Memcached {
    HashTable ht;
    atomic_ullong puts;
    atomic_ullong dels;
    atomic_ullong gets;
    atomic_ullong takes;
};

typedef struct _Memcached *Memcached; 

Memcached memcached_create(unsigned size_hashtable);

int memcached_put(Memcached mc, void* key, unsigned keyLen, void *value, unsigned valueLen);

void memcached_get(Memcached mc, void *key, unsigned keyLen, void **value, unsigned *valueLen);

void memcached_take(Memcached mc, void *key, unsigned keyLen, void **value, unsigned *valueLen);

int memcached_delete(Memcached mc, void *key, unsigned keyLen);

char *memcached_stats(Memcached mc);

int memcached_destroy(Memcached mc);

#endif /* __MEMCACHED_H__ */
