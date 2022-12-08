#include <stdlib.h>
#include "memcached_service.h"

Memcached memcached_create(unsigned size, ComparativeFunctionHash comp,
                         DestructiveFunctionHash destr, HashFunction hash){
    struct rw_lock lock;
    rw_lock_init(&lock);
    HashTable table = create_hashtable(size, comp, destr, custom_alloc);
    Memcached mc = malloc(sizeof(ConcurrentHashTableWithLRU));
    mc->hashtable = table;
    mc->lock = &lock;
    return mc;
}

int memcached_put(Memcached table, void* key, void *data) {
    int idx = table->hashtable->hash(key) % table->hashtable->size;
    concurrent_hashtable_insert(*table, idx, data);
}

void *memcached_get(Memcached table, void *key) {
    int idx = table->hashtable->hash(key) % table->hashtable->size;
    concurrent_hashtable_get(*table, idx, key);
}

void *memcached_take(Memcached table, void *key) {
    int idx = table->hashtable->hash(key) % table->hashtable->size;
    void *res = concurrent_hashtable_take(*table, idx, key);
    return res;
}

int memcached_delete(Memcached table, void *key) {
    int idx = table->hashtable->hash(key) % table->hashtable->size;
    void* res = concurrent_hashtable_take(*table, idx, key);
    if (res == NULL) {
        return 0;
    }
    else {
        return 1;
    }
}

char *memcached_stats(Memcached table);

int memcached_destroy(Memcached table) {
    for (unsigned i = 0; i < table->hashtable->size; i++) {
        list_destroy(table->hashtable->elems[i]);
    }
    //finalizar lock
    free(table);
}
