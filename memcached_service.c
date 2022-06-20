#include <stdlib.h>
#include "concurrent_hash_table.h"
#include "../utils/hash.h"

Memcached memcached_create(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, HashFunction hash){
    struct rw_lock lock;
    rw_lock_init(&lock);
    HashTable table = create_hashtable(size, comp, destr, hash);
    Memcached mc = malloc(sizeof(struct _Memcached));
    mc->hashtable = table;
    mc->lock = &lock;
    mc->lru = dll_create();
    return mc;
}



bool memcached_put(Memcached table, void* key, void *data){
    unsigned keyHash = hash_string((char *)key);
}

void *memcached_get(Memcached table, void *key);

void *memcached_take(Memcached table, void *key);

bool memcached_delete(Memcached table, void *data);

char* memcached_stats(Memcached table);

void memcached_destroy(Memcached table);
