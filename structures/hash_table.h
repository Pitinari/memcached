#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__

#include <stdbool.h>
#include "double_linked_list.h"

/** Retorna una copia fisica del dato */
typedef bool (*ComparativeFunction)(void *data1, void *data2);
/** Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef void (*DestructiveFunction)(void *data);
/** Libera la memoria alocada para el dato */
typedef void *(*AlloccateFunction)(size_t size);
/** malloc personalizado */

/**
 * Estructura principal que representa la tabla hash.
 */
struct _HashTable {
  struct DoubleLinkedList *elems;
  unsigned numElems;
  unsigned size;
  ComparativeFunction comp;
  DestructiveFunction destr;
  AlloccateFunctionDLL custom_malloc;
};

typedef struct _HashTable *HashTable;

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, AlloccateFunctionDLL custom_malloc);

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
