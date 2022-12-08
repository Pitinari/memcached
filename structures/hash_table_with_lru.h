#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__

#include <stdbool.h>
#include "list_with_lru.h"

typedef void *(*AlloccateFunctionHash)(size_t size);
/** Retorna un entero sin signo para el dato */
typedef unsigned (*HashFunction)(void *key);

/**
 * Estructura principal que representa la tabla hash.
 */
struct _HashTable {
  List *lists;
  pthread_mutex_t **lists_locks;
  LRU lru;
  pthread_mutex_t *lru_lock;
  unsigned size;
};

typedef struct _HashTable *HashTable;

struct _NodeHT {
  void *key;
  void *value;
  unsigned hashedKey;
};

typedef struct _NodeHT *NodeHT;

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size);

/**
 * Retorna el numero de elementos de la tabla.
 */
unsigned hashtable_nelems(HashTable table);

/**
 * Retorna la capacidad de la tabla.
 */
unsigned hashtable_size(HashTable table);

/**
 * Destruye la tabla.
 */
void hashtable_destroy(HashTable table);

/**
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void hashtable_insert(HashTable table, void *key, void *value);

/**
 * Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
 * buscado no se encuentra en la tabla.
 */
void *hashtable_search(HashTable table, unsigned hashedValue, void *data);

/**
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void *hashtable_delete(HashTable table, void *key, void *value);

void *custom_malloc(HashTable hashTable, size_t size);

#endif /* __HASHTABLE_H__ */
