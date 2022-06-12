#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__

#include <stdbool.h>

typedef void *(*CopyFunction)(void *data);
/** Retorna una copia fisica del dato */
typedef bool (*ComparativeFunction)(void *data1, void *data2);
/** Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef void (*DestructiveFunction)(void *data);
/** Libera la memoria alocada para el dato */
typedef unsigned (*HashFunction)(void *data, unsigned numCollisions);
/** Retorna un entero sin signo para el dato */

/**
 * Casillas en la que almacenaremos los datos de la tabla hash.
 */
struct HashNode{
  void *data;
  bool eliminated;
};

/**
 * Estructura principal que representa la tabla hash.
 */
struct _HashTable {
  struct HashNode *elems;
  unsigned numElems;
  unsigned size;
  CopyFunction copy;
  ComparativeFunction comp;
  DestructiveFunction destr;
  HashFunction hash;
};

typedef struct _HashTable *HashTable;

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, HashFunction hash,
                         CopyFunction copy);

/**
 * Retorna el numero de elementos de la tabla.
 */
unsigned hashtable_nelems(HashTable table);

/**
 * Retorna la capacidad de la tabla.
 */
unsigned hashtable_capacity(HashTable table);

/**
 * Destruye la tabla.
 */
void hashtable_destroy(HashTable table);

/**
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void hashtable_insert(HashTable table, void *data);

/**
 * Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
 * buscado no se encuentra en la tabla.
 */
void *hashtable_search(HashTable table, void *data);

/**
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void hashtable_delete(HashTable table, void *data);

void *hashtable_to_list (HashTable table);

#endif /* __HASHTABLE_H__ */