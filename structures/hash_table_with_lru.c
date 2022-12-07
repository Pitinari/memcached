#include "hash_table_with_lru.h"

#include <stdlib.h>
#include <math.h>

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunctionHash comp,
													 DestructiveFunctionHash destr, AlloccateFunctionHash custom_malloc) {

	// Pedimos memoria para la estructura principal y las casillas.
	HashTable table = custom_malloc(sizeof(struct _HashTable));
	table->elems = custom_malloc(sizeof(struct _List) * size);
	table->lru = lru_create(custom_malloc, destr, ??, ??, ??);
	table->numElems = 0;
	table->size = size;
	table->comp = comp;
	table->destr = destr;

	// Inicializamos las casillas con datos nulos.
	for (unsigned idx = 0; idx < size; ++idx) {
		table->elems[idx] = list_create(custom_malloc);
	}

	return table;
}

/**
 * Retorna el numero de elementos de la tabla.
 */
unsigned hashtable_nelems(HashTable table) { return table->numElems; }

/**
 * Retorna la capacidad de la tabla.
 */
unsigned hashtable_size(HashTable table) { return table->size; }

/**
 * Destruye la tabla.
 */
void hashtable_destroy(HashTable table) {
	// NodeListLRU node, aux;
	// // Destruir cada uno de los datos.
	// for (unsigned idx = 0; idx < table->size; ++idx){
	//   node = table->elems[idx];
	//   if(node->data == NULL){
	//     free(node);
	//     break;
	//   }
	//   while (node){
	//     table->destr(node->data);
	//     aux = node;
	//     node = node->;
	//     free(aux);
	//   }
	// }
	for (unsigned idx = 0; idx < table->size; ++idx){
		list_destroy(table->elems[idx]);
	}
	lru_destroy(table->lru);
		
	// Liberar el arreglo de casillas y la tabla.
	free(table->elems);
	free(table);
	return;
}

/**
 * Retorna el dato de la tabla que coincida con el dato dado, o NULL si el dato
 * buscado no se encuentra en la tabla.
 */
void *hashtable_search(HashTable table, unsigned hashedValue, void *data) {

	return list_get(table->elems[hashedValue], data, table->comp);
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
