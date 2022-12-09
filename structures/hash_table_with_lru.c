#include <stdlib.h>
#include <math.h>
#include "pthread.h"
#include "string.h"

#include "hash_table_with_lru.h"
#include "utils/hash.h"

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(
  unsigned size,
  HashFunction hash
) {

	// Pedimos memoria para la estructura principal y las casillas.
	HashTable table = malloc(sizeof(struct _HashTable));
  if(!table) goto error1;
	table->lists = malloc(sizeof(struct _List) * size);
  if(!table->lists) goto error2;
  int i = 0;
  while(i < size){
    table->lists[i] = list_create();
    if(!table->lists[i]) { i--; goto error3; }
    i++;
  }
  table->lists_locks = malloc(sizeof(pthread_mutex_t) * size);
  if(!table->lists_locks) goto error4;

  table->lru_lock = malloc(sizeof(pthread_mutex_t));
  if(!table->lru_lock) goto error5;

  for(int j = 0; j < size; j++) {
    pthread_mutex_init(table->lists_locks[j], NULL);
  }

	table->lru = lru_create(
    custom_malloc_wrapper, 
    nodeht_destroy, 
    lru_preprocessing, 
    lru_postprocessing,
    on_add_element,
    on_delete_element,
    (void *)table
  );
  if(!table->lru) goto error6;

	table->size = size;
	table->numElems = ATOMIC_VAR_INIT(0);
  table->hash = hash;

	return table;

  error6:
  free(table->lru_lock);

  error5:
  free(table->lists_locks);

  error4:
  i--;

  error3:
  for(; i >= 0; i--) {
    list_destroy(table->lists[i]);
  }
  error2:
  free(table);
  error1:
  return NULL;
}

void on_add_element(void *hashTable){
  atomic_fetch_add(&((HashTable)hashTable)->numElems, 1);
}

void on_delete_element(void *hashTable){
  atomic_fetch_sub(&((HashTable)hashTable)->numElems, 1);
}

bool comparate_keys(void *data1, void *data2){
  NodeHT a = (NodeHT)data1, b = (NodeHT)data2;
  if(
    a->keyLen == b->keyLen &&
    a->hashedKey == b->hashedKey
  ){
    return memcmp(a->key, b->key, a->keyLen) == 0 ? true : false;
  } 
  return false;
}

NodeHT nodeht_create(
  HashTable hashTable, 
  void *key,
  unsigned keyLen,
  void *value, 
  unsigned hashedKey
){
  NodeHT node = custom_malloc(hashTable, sizeof (struct _NodeHT));
  node->key = key;
  node->keyLen = keyLen;
  node->value = value;
  node->hashedKey = hashedKey;
  return node;
}

void nodeht_destroy(
  NodeHT node
){
  free(node->key);
  free(node->value);
  free(node);
}

void *custom_malloc(HashTable hashTable, size_t size){
  return custom_malloc_wrapper((void *)hashTable, size, NULL);
}

void *custom_malloc_wrapper(void *hashTable, size_t size, List currentList){
  void *mem;
  while(mem = malloc(size)) {
    pthread_mutex_lock(&((HashTable)hashTable)->lru_lock);
    lru_deallocate(((HashTable)hashTable)->lru, currentList);
    pthread_mutex_unlock(&((HashTable)hashTable)->lru_lock);
  }
}

List lru_preprocessing(void *hashTable, void *data, List currentList){
  int idx = ((NodeHT)data)->hashedKey % ((HashTable)hashTable)->size;
  List list = ((HashTable)hashTable)->lists[idx];
  if(list != currentList) {
    pthread_mutex_lock(&((HashTable)hashTable)->lists_locks[idx]);
  }
  return list;
}

void lru_postprocessing(void *hashTable, void *data, List currentList){
  int idx = ((NodeHT)data)->hashedKey % ((HashTable)hashTable)->size;
  List list = ((HashTable)hashTable)->lists[idx];
  if(list != currentList) {
    pthread_mutex_unlock(&((HashTable)hashTable)->lists_locks[idx]);
  }
}

/**
 * Destruye la tabla.
 */
void hashtable_destroy(HashTable table) {
	for(int i = 0; i < table->size; i++){
    list_destroy(table->lists[i]);
  }
  lru_destroy(table->lru);
  free(table->lists);
  free(table->lists_locks);
  free(table->lru_lock);
  free(table);
}

/**
 * Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
 * buscado no se encuentra en la tabla.
 */
void *hashtable_search(HashTable table, void *key, unsigned keyLen) {
  unsigned hashedValue = table->hash(key);
  unsigned idx = hashedValue % table->size;
  struct _NodeHT data = { key, keyLen, NULL, hashedValue };
  pthread_mutex_lock(table->lists_locks[idx]);
	NodeHT returnValue = (NodeHT)list_get(table->lists[idx], (void *)&data, comparate_keys);
  pthread_mutex_unlock(table->lists_locks[idx]);
  return returnValue ? returnValue->value : NULL;
}

/**
 * tablahash_insertar: TablaHash *void -> void
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void hashtable_insert(HashTable table, void *key, unsigned keyLen, void *value) {
  unsigned hashedValue = table->hash(key);
  unsigned idx = hashedValue % table->size;
  NodeHT data = nodeht_create(table, key, keyLen, value, hashedValue);
  pthread_mutex_lock(table->lists_locks[idx]);
	list_put(table->lists[idx], table->lru, (void *)&data, comparate_keys);
  pthread_mutex_unlock(table->lists_locks[idx]);
}

/**
 * tablahash_eliminar: TablaHash *void -> void
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void *hashtable_delete(HashTable table, void *key, unsigned keyLen) {
  unsigned hashedValue = table->hash(key);
  unsigned idx = hashedValue % table->size;
  struct _NodeHT data = { key, keyLen, NULL, hashedValue };
  pthread_mutex_lock(table->lists_locks[idx]);
	NodeHT returnValue = (NodeHT)list_delete(table->lists[idx], table->lru, (void *)&data, comparate_keys);
  pthread_mutex_unlock(table->lists_locks[idx]);
  return returnValue;
}
