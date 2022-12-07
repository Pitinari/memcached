#ifndef __CONCURRENTHASHTABLEWITHLRU_H__
#define __CONCURRENTHASHTABLEWITHLRU_H__

#include "hash_table_with_lru.h"

// Estructura de tabla hash con lock
struct _ConcurrentHashTableWithLRU{
	HashTable hashtable;
	pthread_mutex_t tableLock;
	struct rw_lock **lock;
};

typedef unsigned (*HashFunction)(void *data);
/** Retorna un entero sin signo para el dato */

typedef struct _ConcurrentHashTableWithLRU ConcurrentHashTableWithLRU;

ConcurrentHashTableWithLRU create_concurrent_hashtable(
	unsigned size, 
	ComparativeFunctionHash comp, 
	DestructiveFunctionHash destr
);

/**
 * Retorna el numero de elementos de la tabla.
 */
unsigned concurrent_hashtable_nelems(ConcurrentHashTableWithLRU table);

/**
 * Retorna la capacidad de la tabla.
 */
unsigned concurrent_hashtable_capacity(ConcurrentHashTableWithLRU table);

/**
 * Destruye la tabla.
 */
void concurrent_hashtable_destroy(ConcurrentHashTableWithLRU table);

/**
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void concurrent_hashtable_insert(ConcurrentHashTableWithLRU table, unsigned hashedValue, void *data);

/**
 * Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
 * buscado no se encuentra en la tabla.
 */
void *concurrent_hashtable_search(ConcurrentHashTableWithLRU table, unsigned hashedValue, void *data);

/**
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void concurrent_hashtable_delete(ConcurrentHashTableWithLRU table, unsigned hashedValue, void *data);

void *concurrent_hashtable_to_list (ConcurrentHashTableWithLRU table);

#endif /* __CONCURRENTHASHTABLEWITHLRU_H__ */