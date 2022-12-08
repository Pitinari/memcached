#include "hash_table_with_lru.h"

#include <stdlib.h>
#include <math.h>
#include "pthread.h"

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size) {

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
    (void *)table
  );
  if(!table->lru) goto error6;

	table->size = size;

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

NodeHT nodeht_create(
  HashTable hashTable, 
  void *key, 
  void *value, 
  unsigned hashedKey
){
  NodeHT node = custom_malloc(hashTable, sizeof (struct _NodeHT));
  node->key = key;
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
 * Retorna la capacidad de la tabla.
 */
unsigned hashtable_size(HashTable table) { return table->size; }

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
void *hashtable_search(HashTable table, void *key, void *value) {

	return list_get(table->lists[hashedValue], data, table->comp);
}

/**
 * tablahash_insertar: TablaHash *void -> void
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void hashtable_insert(HashTable table, unsigned hashedValue, void *data) {

	list_put(table->elems[hashedValue], table->lru, data, custom_malloc, table->comp, table->destr);
	table->numElems++;
}

/**
 * tablahash_eliminar: TablaHash *void -> void
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void hashtable_delete(HashTable table, unsigned hashedValue, void *data) {

	list_delete(table->elems[hashedValue], table->lru, data, table->comp);

}
