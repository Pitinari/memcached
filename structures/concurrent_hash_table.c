#include <stdlib.h>
#include "concurrent_hash_table.h"

ConcurrentHashTable create_concurrent_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, HashFunction hash,
                         CopyFunction copy){
    struct rw_lock lock;
    rw_lock_init(&lock);
    HashTable table = create_hashtable(size, comp, destr, hash, copy);
    ConcurrentHashTable concTable;
    concTable->hashtable = table;
    concTable->lock = &lock;
    return concTable; 
}

unsigned concurrent_hashtable_nelems(ConcurrentHashTable table) { 
    lock_readers(table->lock);
    unsigned numElems = table->hashtable->numElems;
    unlock_readers(table->lock);
    return numElems; 
}

unsigned concurrent_hashtable_capacity(ConcurrentHashTable table) { 
    lock_readers(table->lock);
    unsigned capacity = table->hashtable->size;
    unlock_readers(table->lock);
    return capacity;
}

void concurrent_hashtable_destroy(ConcurrentHashTable table){
    // TODO: destroy queue
    hashtable_destroy(table);
}

void concurrent_hashtable_insert(ConcurrentHashTable table, void *data){
    lock_writers(table->lock);
    hashtable_insert(table->hashtable, data);
    unlock_writers(table->lock);
}

void *concurrent_hashtable_search(ConcurrentHashTable table, void *data){
    lock_readers(table->lock);
    void *response = hashtable_search(table->hashtable);
    unlock_readers(table->lock);
    return response;
}

void concurrent_hashtable_delete(ConcurrentHashTable table, void *data){
    lock_writers(table->lock);
    hashtable_delete(table->hashtable);
    unlock_writers(table->lock);
}

void *concurrent_hashtable_to_list (ConcurrentHashTable table){
    lock_readers(table->lock);
    hashtable_to_list(table->hashtable, data);
    unlock_readers(table->lock);
}