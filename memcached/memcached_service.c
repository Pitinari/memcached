#include <stdlib.h>
#include <stdio.h>
#include "memcached_service.h"

Memcached memcached_create(unsigned size){
	Memcached mc = malloc(sizeof(struct _Memcached));
	if(!mc) goto error1;
	mc->ht = create_hashtable(size);
	if(!mc->ht) goto error2;

	// Variables para sumar y restar atomicamente
	mc->puts = ATOMIC_VAR_INIT(0);
	mc->dels = ATOMIC_VAR_INIT(0);
	mc->gets = ATOMIC_VAR_INIT(0);
	mc->takes = ATOMIC_VAR_INIT(0);

	return mc;

	error2:
	free(mc);
	error1:
	return NULL;
}

bool memcached_put(Memcached mc, void* key, unsigned keyLen, void *value, unsigned valueLen) {
	atomic_fetch_add(&mc->puts, 1);
	return hashtable_insert(mc->ht, key, keyLen, value, valueLen);
}

void memcached_get(Memcached mc, void *key, unsigned keyLen, void **value, unsigned *valueLen) {
	atomic_fetch_add(&mc->gets, 1);
	hashtable_search(mc->ht, key, keyLen, value, valueLen);
}

// si existe el nodo, guarda el valor y su largo en sus respectivos punteros y libera el resto del nodo 
void memcached_take(Memcached mc, void *key, unsigned keyLen, void **value, unsigned *valueLen) {
	atomic_fetch_add(&mc->takes, 1);
	NodeHT node = hashtable_take(mc->ht, key, keyLen);
	if (node) {
		*value = node->value;
		*valueLen = node->valueLen;
		free(node->key);
		free(node);
	}
}

int memcached_delete(Memcached mc, void *key, unsigned keyLen) {
	atomic_fetch_add(&mc->dels, 1);
	NodeHT node = hashtable_take(mc->ht, key, keyLen);
	if(node) {
		nodeht_destroy(node);
		return 0;
	}
	return 1;
}

void memcached_stats(Memcached mc, char *buffer){
	sprintf(
		buffer, 
		"PUTS=%llu DELS=%llu TAKES=%llu GETS=%llu KEYS=%llu", 
		mc->puts, mc->dels, mc->takes, mc->gets, mc->ht->numElems);
}

int memcached_destroy(Memcached mc) {
	hashtable_destroy(mc->ht);
	free(mc);
}
