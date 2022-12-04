#include "hash_table.h"

#include <stdlib.h>
#include <math.h>

#define RESIZE_FACTOR 5

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, AlloccateFunctionDLL custom_malloc) {

  // Pedimos memoria para la estructura principal y las casillas.
  HashTable table = custom_malloc(sizeof(struct _HashTable));
  table->elems = custom_malloc(sizeof(struct _DoubleLinkedList) * size);
  table->numElems = 0;
  table->size = size;
  table->comp = comp;
  table->destr = destr;
  table->custom_malloc = custom_malloc;

  // Inicializamos las casillas con datos nulos.
  for (unsigned idx = 0; idx < size; ++idx) {
    table->elems[idx] = dll_create(custom_malloc);
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
  NodeDLL node, aux;
  // Destruir cada uno de los datos.
  for (unsigned idx = 0; idx < table->size; ++idx){
    node = table->elems[idx];
    if(node->data == NULL){
      free(node);
      break;
    }
    while (node){
      table->destr(node->data);
      aux = node;
      node = node->next;
      free(aux);
    }
    
  }
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

  return dll_search(table->elems[hashedValue], table->comp);
}

/**
 * tablahash_insertar: TablaHash *void -> void
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void hashtable_insert(HashTable table, unsigned hashedValue, void *data) {

  dll_insert(table->elems[hashedValue], data, table->comp, table->destr);
  table->numElems++;
}

/**
 * tablahash_eliminar: TablaHash *void -> void
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void hashtable_delete(HashTable table, unsigned hashedValue, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr) {

  void dll_node_delete(table->elems[hashedValue], data, comp, destr);

}
