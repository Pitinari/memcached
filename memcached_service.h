#ifndef __MEMCACHED_H__
#define __MEMCACHED_H__

#include "./structures/hash_table_with_lru.h"

struct _Memcached {
    HashTable ht;
    atomic_uint puts;
    atomic_uint dels;
    atomic_uint gets;
    atomic_uint takes;
};

typedef struct _Memcached *Memcached; 

Memcached memcached_create(
    unsigned size_hashtable, 
    ComparativeFunction comp_hashtable,
    HashFunction hash
);

int memcached_put(Memcached mc, void* key, unsigned keyLen, void *data);

void *memcached_get(Memcached mc, void *key, unsigned keyLen);

void *memcached_take(Memcached mc, void *key, unsigned keyLen);

int memcached_delete(Memcached mc, void *key, unsigned keyLen);

char *memcached_stats(Memcached mc);

int memcached_destroy(Memcached mc);

#endif /* __MEMCACHED_H__ */
