#include <stdlib.h>
#include <stdio.h>
#include "memcached_service.h"

Memcached memcached_create(unsigned size){
	Memcached mc = malloc(sizeof(struct _Memcached));
	if(!mc) goto error1;
	mc->ht = create_hashtable(size);
	if(!mc->ht) goto error2;
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

int memcached_put(Memcached mc, void* key, unsigned keyLen, void *value) {
	atomic_fetch_add(&mc->puts, 1);
	hashtable_insert(mc->ht, key, keyLen, value);
	return 0;
}

void *memcached_get(Memcached mc, void *key, unsigned keyLen) {
	atomic_fetch_add(&mc->gets, 1);
	return hashtable_search(mc->ht, key, keyLen);
}

void *memcached_take(Memcached mc, void *key, unsigned keyLen) {
	atomic_fetch_add(&mc->takes, 1);
	NodeHT node = hashtable_take(mc->ht, key, keyLen);
	if (node) {
		void *value = node->value;
		free(node->key);
		free(node);
		return value;
	}
	return NULL;
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

char *memcached_stats(Memcached mc){
	char *str = custom_malloc(mc->ht, 100);
	sprintf(
		str, 
		"OK PUTS=%u DELS=%u TAKES=%u GETS=%u KEYS=%u\n", 
		mc->puts, mc->dels, mc->takes, mc->gets, mc->ht->numElems);
	return str;
}

int memcached_destroy(Memcached mc) {
	hashtable_destroy(mc->ht);
	free(mc);
}
