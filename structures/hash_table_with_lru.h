#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__

#include <stdbool.h>
#include <stdatomic.h>
#include "pthread.h"
#include "list_with_lru.h"

/* Retorna un entero sin signo para el dato */
typedef void *(*AlloccateFunctionHash) (size_t size);

/* Estructura principal que representa la tabla hash */
struct _HashTable {
  List *lists;
  pthread_mutex_t **lists_locks;
  LRU lru;
  pthread_mutex_t *lru_lock;
  unsigned size;
  atomic_ullong numElems;
};
/* Estructura principal que representa la tabla hash */
typedef struct _HashTable *HashTable;

/* Nodo de la hash table */
struct _NodeHT {
  void *key;
  unsigned keyLen;
  void *value;
  unsigned valueLen;
  unsigned hashedKey;
};
/* Nodo de la hash table */
typedef struct _NodeHT *NodeHT;

/* Crea una nueva tabla hash vacia, con la capacidad dada */
HashTable create_hashtable(unsigned size);

/* Retorna el numero de elementos de la tabla */
unsigned hashtable_nelems(HashTable table);

/* Destruye la tabla */
void hashtable_destroy(HashTable table);

/* Inserta un dato en la tabla, o lo reemplaza si ya se encontraba */
bool hashtable_insert(HashTable table, void *key, unsigned keyLen, void *value, unsigned valueLen);

/*Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
buscado no se encuentra en la tabla */
void hashtable_search(HashTable table, void *key, unsigned keyLen, void **value, unsigned *valueLen);

/* Elimina el dato de la tabla que coincida con el dato dado */
void *hashtable_take(HashTable table, void *key, unsigned keyLen);

/* Funcion personal para allocar memoria */
void *custom_malloc(HashTable hashTable, size_t size);

/* Funcion para crear nodos de la hash table */
NodeHT nodeht_create(HashTable hashTable, void *key, unsigned keyLen,
											void *value, unsigned valueLen, unsigned hashedKey);

/* Libera la memoria del nodo */
void nodeht_destroy(NodeHT node);

#endif /* __HASHTABLE_H__ */
