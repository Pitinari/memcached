#ifndef __CONCURRENTHASHTABLE_H__
#define __CONCURRENTHASHTABLE_H__

#include "hash_table.h"
#include "../structure_lock/rwlock.h"

// Estructura de tabla hash con lock
struct _ConcurrentHashTable{
    HashTable hashtable;
    struct rw_lock *lock;
};

typedef struct _ConcurrentHashTable ConcurrentHashTable;

ConcurrentHashTable create_concurrent_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, HashFunction hash,
                         CopyFunction copy);

/**
 * Retorna el numero de elementos de la tabla.
 */
unsigned concurrent_hashtable_nelems(ConcurrentHashTable table);

/**
 * Retorna la capacidad de la tabla.
 */
unsigned concurrent_hashtable_capacity(ConcurrentHashTable table);

/**
 * Destruye la tabla.
 */
void concurrent_hashtable_destroy(ConcurrentHashTable table);

/**
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void concurrent_hashtable_insert(ConcurrentHashTable table, void *data);

/**
 * Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
 * buscado no se encuentra en la tabla.
 */
void *concurrent_hashtable_search(ConcurrentHashTable table, void *data);

/**
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void concurrent_hashtable_delete(ConcurrentHashTable table, void *data);

void *concurrent_hashtable_to_list (ConcurrentHashTable table);

#endif /* __CONCURRENTHASHTABLE_H__ */