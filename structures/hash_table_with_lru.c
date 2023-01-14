#include <stdlib.h>
#include <math.h>
#include <stdio.h>
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

void take_lru_lock(void *hashTable){
	pthread_mutex_lock(((HashTable)hashTable)->lru_lock);
}

void drop_lru_lock(void *hashTable){
	pthread_mutex_unlock(((HashTable)hashTable)->lru_lock);
}

// Esta funcion se pasa a la estructura de la LRU para borrar las listas si falla algun malloc
// Intenta hacer malloc hasta 3 veces o hasta que la lista este vacia
void *custom_malloc_wrapper(void *hashTable, size_t size, List currentList) {
	void *mem;
	int numberTries = 0;
	bool removed = true;
	again:
	mem = malloc(size);
	if(mem == NULL && numberTries < 3 && removed) {
		pthread_mutex_lock(((HashTable)hashTable)->lru_lock);
		fprintf(stderr, "DEALLOCATING...");
		removed = lru_deallocate(((HashTable)hashTable)->lru, currentList);
		fprintf(stderr, "Remaining elements %llu\n", ((HashTable)hashTable)->numElems);
		pthread_mutex_unlock(((HashTable)hashTable)->lru_lock);
		numberTries++;
		goto again;
	}
	return mem;
}

// Esta funcion es la que va a exportar la libreria para utilizar
// currentList es NULL ya que cuando se use ningun lock de lista va a estar tomado
void *custom_malloc(HashTable hashTable, size_t size) {
	return custom_malloc_wrapper((void *)hashTable, size, NULL);
}

// preprocessing toma la estructura, un nodo y la lista actual cuando se llamo a
// deallocate. Si la lista del nodo es la actual, no se pide el lock
List lru_preprocessing(void *hashTable, void *data, List currentList) {
	int idx = ((NodeHT)data)->hashedKey % ((HashTable)hashTable)->size;
	List list = ((HashTable)hashTable)->lists[idx];
	if(list != currentList) {
		if(pthread_mutex_trylock(&((HashTable)hashTable)->lists_locks[idx]) < 0){			
			return NULL;
		}
	}
	return list;
}

// postprocessing realiza la misma logica que preprocessing pero liberando el lock
void lru_postprocessing(void *hashTable, void *data, List currentList) {
	int idx = ((NodeHT)data)->hashedKey % ((HashTable)hashTable)->size;
	List list = ((HashTable)hashTable)->lists[idx];
	if(list != currentList) {
		pthread_mutex_unlock(&((HashTable)hashTable)->lists_locks[idx]);
	}
}

// funcion destructora pero con el nodo como puntero void para pasar a la estructura
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
	table->lists_locks = malloc(sizeof(table->lists_locks[0]) * size);
	if (!table->lists_locks) goto error4;

	table->lru_lock = malloc(sizeof(pthread_mutex_t));
	if (!table->lru_lock) goto error5;

	for (; j < size; j++) {
		pthread_mutex_init(&table->lists_locks[j], NULL);
	}

	pthread_mutex_init(table->lru_lock, NULL);

	table->lru = lru_create(
		custom_malloc_wrapper, 
		nodeht_destroy_wrapper, 
		lru_preprocessing, 
		lru_postprocessing, 
		on_add_element, 
		on_delete_element,
		take_lru_lock,
		drop_lru_lock, 
		(void *)table
	);
	if (!table->lru) goto error7;

	table->size = size;
	table->numElems = ATOMIC_VAR_INIT(0);

	return table;

	/* Resolucion de errores */
	error7:
	free(table->lru_lock);
	j--;

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
bool compare_keys(void *data1, void *data2) {
	NodeHT a = (NodeHT)data1, b = (NodeHT)data2;
	if (a->keyLen == b->keyLen &&
			a->hashedKey == b->hashedKey) {
		return memcmp(a->key, b->key, a->keyLen) == 0;
	}
	return false;
}

NodeHT nodeht_create(
	HashTable hashTable, 
	void *key, 
	unsigned keyLen, 
	void *value,
	unsigned valueLen,
	unsigned hashedKey
) {
	NodeHT node = custom_malloc(hashTable, sizeof (struct _NodeHT));
	if (node != NULL)	{
		node->key = key;
		node->keyLen = keyLen;
		node->value = value;
		node->valueLen = valueLen;
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

// si existe el value cambia se lo asigna al puntero pasado como value, junto con su largo
void hashtable_search(HashTable table, void *key, unsigned keyLen, void **value, unsigned *valueLen) {
	if (table == NULL) return;
	unsigned hashedKey = hash_function(key, keyLen);
	unsigned idx = hashedKey % table->size;
	struct _NodeHT data = { key, keyLen, NULL, 0, hashedKey };
	pthread_mutex_lock(&table->lists_locks[idx]);
	NodeHT returnValue = (NodeHT)list_get(table->lists[idx], (void *)&data, compare_keys);
	if(returnValue){
		*value = custom_malloc(table, returnValue->valueLen);
		if(*value == NULL){
			pthread_mutex_unlock(&table->lists_locks[idx]);
			return;
		}
		memcpy(*value, returnValue->value, returnValue->valueLen);
		*valueLen = returnValue->valueLen;
	} else {
		*value = NULL;
		*valueLen = 0;
	}
	pthread_mutex_unlock(&table->lists_locks[idx]);
	return;
}

bool hashtable_insert(HashTable table, void *key, unsigned keyLen, void *value, unsigned valueLen) {
	if (table == NULL) return false;
	unsigned hashedKey = hash_function(key, keyLen);
	unsigned idx = hashedKey % table->size;
	NodeHT data = nodeht_create(table, key, keyLen, value, valueLen, hashedKey);
	if(data == NULL) return false;
	pthread_mutex_lock(&table->lists_locks[idx]);
	bool result = list_put(table->lists[idx], table->lru, (void *)data, compare_keys);
	pthread_mutex_unlock(&table->lists_locks[idx]);
	return result;
}

void *hashtable_take(HashTable table, void *key, unsigned keyLen) {
	if (table == NULL) return NULL;
	unsigned hashedValue = hash_function(key, keyLen);
	unsigned idx = hashedValue % table->size;
	struct _NodeHT data = { key, keyLen, NULL, 0, hashedValue };
	pthread_mutex_lock(&table->lists_locks[idx]);
	NodeHT returnValue = (NodeHT)list_take(table->lists[idx], table->lru, (void *)&data, compare_keys);
	pthread_mutex_unlock(&table->lists_locks[idx]);
	return returnValue;
}
