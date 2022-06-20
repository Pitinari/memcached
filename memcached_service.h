#ifndef __MEMCACHED_H__
#define __MEMCACHED_H__

#include "hash_table.h"
#include "../structure_lock/rwlock.h"

// Estructura de tabla hash con lock
struct _Memcached{
    HashTable hashtable;
    struct rw_lock *lock;
    DoubleLinkedList lru;
};

typedef struct _Memcached *Memcached;

Memcached memcached_create(unsigned size_hashtable, ComparativeFunction comp_hashtable,
                         DestructiveFunction destr_hashtable, HashFunction hash);

bool memcached_put(Memcached table, void* key, void *data);

void *memcached_get(Memcached table, void *key);

void *memcached_take(Memcached table, void *key);

bool memcached_delete(Memcached table, void *data);

char* memcached_stats(Memcached table);

void memcached_destroy(Memcached table);

#endif /* __MEMCACHED_H__ */
