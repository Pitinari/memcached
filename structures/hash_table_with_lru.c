#include <stdlib.h>
#include <math.h>
#include "string.h"
#include "hash_table_with_lru.h"
#include "../utils/hash.h"

/* Funcion para incrementar en 1 el contador de elementos */
void on_add_element(void *hashTable) {
	atomic_fetch_add(&((HashTable)hashTable)->numElems, 1);
}

/* Funcion para decrementar en 1 el contador de elementos */
void on_delete_element(void *hashTable) {
	atomic_fetch_sub(&((HashTable)hashTable)->numElems, 1);
}

void *custom_malloc_wrapper(void *hashTable, size_t size, List currentList) {
	void *mem;
	int numberTries = 0;
	bool removed = true;
	while((mem = malloc(size)) == NULL && numberTries < 3 && removed) {
		pthread_mutex_lock(((HashTable)hashTable)->lru_lock);
		removed = lru_deallocate(((HashTable)hashTable)->lru, currentList);
		pthread_mutex_unlock(((HashTable)hashTable)->lru_lock);
		numberTries++;
	}
	return mem;
}

void *custom_malloc(HashTable hashTable, size_t size) {
	return custom_malloc_wrapper((void *)hashTable, size, NULL);
}

List lru_preprocessing(void *hashTable, void *data, List currentList) {
	int idx = ((NodeHT)data)->hashedKey % ((HashTable)hashTable)->size;
	List list = ((HashTable)hashTable)->lists[idx];
	if(list != currentList) {
		pthread_mutex_lock(((HashTable)hashTable)->lists_locks[idx]);
	}
	return list;
}

void lru_postprocessing(void *hashTable, void *data, List currentList) {
	int idx = ((NodeHT)data)->hashedKey % ((HashTable)hashTable)->size;
	List list = ((HashTable)hashTable)->lists[idx];
	if(list != currentList) {
		pthread_mutex_unlock(((HashTable)hashTable)->lists_locks[idx]);
	}
}

void nodeht_destroy_wrapper(void *node) {
	nodeht_destroy((NodeHT)node);
}

void nodeht_destroy(NodeHT node) {
	if (node != NULL) {	
		free(node->key);
		free(node->value);
		free(node);
	}
}

HashTable create_hashtable(unsigned size) {
	/* Pedimos memoria para la estructura principal y las casillas */
	HashTable table = malloc(sizeof(struct _HashTable));
	if (!table) goto error1;
	table->lists = malloc(sizeof(struct _List) * size);
	if (!table->lists) goto error2;
	int i = 0, j = 0;
	while (i < size) {
		table->lists[i] = list_create();
		if(!table->lists[i]) { i--; goto error3; }
		i++;
	}
	table->lists_locks = malloc(sizeof(pthread_mutex_t *) * size);
	if (!table->lists_locks) goto error4;

	table->lru_lock = malloc(sizeof(pthread_mutex_t));
	if (!table->lru_lock) goto error5;

	for (; j < size; j++) {
		if ((table->lists_locks[j] = malloc(sizeof(pthread_mutex_t))) == NULL) {
			j--;
			goto error6;
		}
		pthread_mutex_init(table->lists_locks[j], NULL);
	}

	pthread_mutex_init(table->lru_lock, NULL);

	table->lru = lru_create(custom_malloc_wrapper, nodeht_destroy_wrapper, 
													lru_preprocessing, lru_postprocessing,
													on_add_element, on_delete_element, (void *)table);
	if (!table->lru) goto error7;

	table->size = size;
	table->numElems = ATOMIC_VAR_INIT(0);

	return table;

	/* Resolucion de errores */
	error7:
	free(table->lru_lock);
	j--;

	error6:
	for (; j >= 0; j--) {
		free(table->lists_locks[i]);
	}

	error5:
	free(table->lists_locks);

	error4:
	i--;

	error3:
	for (; i >= 0; i--) {
		list_destroy(table->lists[i]);
	}
	error2:
	free(table);
	error1:
	return NULL;
}

/* Funcion para comparar las claves */
bool comparate_keys(void *data1, void *data2) {
	if (data1 == NULL || data2 == NULL)	{
		return false;
	}
	NodeHT a = (NodeHT)data1, b = (NodeHT)data2;
	if (a->keyLen == b->keyLen &&
			a->hashedKey == b->hashedKey) {
		return memcmp(a->key, b->key, a->keyLen) == 0;
	}
	return false;
}

NodeHT nodeht_create(HashTable hashTable, void *key, unsigned keyLen,
											void *value, unsigned hashedKey) {
	NodeHT node = custom_malloc(hashTable, sizeof (struct _NodeHT));
	if (node != NULL)	{
		node->key = key;
		node->keyLen = keyLen;
		node->value = value;
		node->hashedKey = hashedKey;
	}	
	return node;
}

void hashtable_destroy(HashTable table) {
	if (table != NULL) {
		/* Liberamos las listas y la lru */
		for (int i = 0; i < table->size; i++) {
			list_destroy(table->lists[i]);
		}
		lru_destroy(table->lru);
		free(table->lists);
		free(table->lists_locks);
		free(table->lru_lock);
		free(table);
	}	
}

void *hashtable_search(HashTable table, void *key, unsigned keyLen) {
	if (table == NULL) return NULL; 
	unsigned hashedKey = hash_function(key, keyLen);
	unsigned idx = hashedKey % table->size;
	struct _NodeHT data = { key, keyLen, NULL, hashedKey };
	pthread_mutex_lock(table->lists_locks[idx]);
	NodeHT returnValue = (NodeHT)list_get(table->lists[idx], (void *)&data, comparate_keys);
	pthread_mutex_unlock(table->lists_locks[idx]);
	return returnValue ? returnValue->value : NULL;
}

void hashtable_insert(HashTable table, void *key, unsigned keyLen, void *value) {
	if (table == NULL) return; 
	unsigned hashedKey = hash_function(key, keyLen);
	unsigned idx = hashedKey % table->size;
	NodeHT data = nodeht_create(table, key, keyLen, value, hashedKey);
	pthread_mutex_lock(table->lists_locks[idx]);
	list_put(table->lists[idx], table->lru, (void *)data, comparate_keys);
	pthread_mutex_unlock(table->lists_locks[idx]);
}

void *hashtable_take(HashTable table, void *key, unsigned keyLen) {
	if (table == NULL) return NULL; 
	unsigned hashedValue = hash_function(key, keyLen);
	unsigned idx = hashedValue % table->size;
	struct _NodeHT data = { key, keyLen, NULL, hashedValue };
	pthread_mutex_lock(table->lists_locks[idx]);
	NodeHT returnValue = (NodeHT)list_take(table->lists[idx], table->lru, (void *)&data, comparate_keys);
	pthread_mutex_unlock(table->lists_locks[idx]);
	return returnValue;
}
