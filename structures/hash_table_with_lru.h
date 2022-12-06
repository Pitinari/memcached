#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__

#include <stdbool.h>
#include "list_with_lru.h"

/** Retorna una copia fisica del dato */
typedef void* (*CopyFunction)(void * data);
/** Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef bool (*ComparativeFunction)(void *data1, void *data2);
/** Libera la memoria alocada para el dato */
typedef void (*DestructiveFunction)(void *data);
/** malloc personalizado */
typedef void *(*AlloccateFunction)(size_t size);
/** Retorna un entero sin signo para el dato */
typedef unsigned (*HashFunction)(void *key);

/**
 * Estructura principal que representa la tabla hash.
 */
struct _HashTable {
  List *elems;
  LRU lru;
  unsigned numElems;
  unsigned size;
  ComparativeFunction comp;
  DestructiveFunction destr;
  AlloccateFunction custom_malloc;
  HashFunction hash;
};

typedef struct _HashTable *HashTable;

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, AlloccateFunction custom_malloc);

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
void hashtable_insert(HashTable table, unsigned hashedValue, void *data);

/**
 * Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
 * buscado no se encuentra en la tabla.
 */
void *hashtable_search(HashTable table, unsigned hashedValue, void *data);

/**
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void hashtable_delete(HashTable table, unsigned hashedValue, void *data);

#endif /* __HASHTABLE_H__ */
