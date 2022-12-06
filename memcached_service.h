#ifndef __MEMCACHED_H__
#define __MEMCACHED_H__

#include "./structures/concurrent_hash_table_with_lru.h"

typedef ConcurrentHashTableWithLRU* Memcached; 

Memcached memcached_create(unsigned size_hashtable, CopyFunction copy, ComparativeFunction comp_hashtable,
                         DestructiveFunction destr_hashtable, HashFunction hash);

int memcached_put(Memcached table, void* key, void *data);

void *memcached_get(Memcached table, void *key);

void *memcached_take(Memcached table, void *key);

int memcached_delete(Memcached table, void *data);

char *memcached_stats(Memcached table);

int memcached_destroy(Memcached table);

#endif /* __MEMCACHED_H__ */
