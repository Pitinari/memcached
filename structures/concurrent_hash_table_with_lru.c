#include <stdlib.h>
#include "concurrent_hash_table_with_lru.h"

ConcurrentHashTableWithLRU create_concurrent_hashtable(
	unsigned size, 
	ComparativeFunction comp, 
	DestructiveFunction destr, 
	CopyFunction copy
){
	struct rw_lock lock[size];
	for(int i = 0; i < size; i++){
		rw_lock_init(&(lock[i]));
	}
	pthread_cond_t numElemsLock;
	pthread_mutex_init(&numElemsLock, NULL);
	HashTable table = create_hashtable(size, comp, destr, copy);
	ConcurrentHashTableWithLRU concTable;
	concTable->tableLock = numElemsLock;
	concTable->hashtable = table;
	concTable->lock = &lock;
	return concTable; 
}

unsigned concurrent_hashtable_nelems(ConcurrentHashTableWithLRU table) {
	return table->hashtable->numElems;
}

unsigned concurrent_hashtable_size(ConcurrentHashTableWithLRU table) {
	return table->hashtable->size;
}

void concurrent_hashtable_insert(ConcurrentHashTableWithLRU table, unsigned hashedValue, void *data){
	lock_writers(table->lock);
	hashtable_insert(table->hashtable, hashedValue, data);
	unlock_writers(table->lock);
}

void *concurrent_hashtable_search(ConcurrentHashTableWithLRU table, unsigned hashedValue, void *data){
	lock_readers(table->lock[hashedValue]);
	void *response = hashtable_search(table->hashtable, hashedValue, data);
	unlock_readers(table->lock[hashedValue]);
	return response;
}

void concurrent_hashtable_delete(ConcurrentHashTableWithLRU table, unsigned hashedValue, void *data){
	lock_writers(table->lock[hashedValue]);
	hashtable_delete(table->hashtable, hashedValue, data);
	unlock_writers(table->lock[hashedValue]);
}

void *concurrent_hashtable_to_list (ConcurrentHashTableWithLRU table){
	lock_readers(table->lock[hashedValue]);
	hashtable_to_list(table->hashtable, data);
	unlock_readers(table->lock[hashedValue]);
}