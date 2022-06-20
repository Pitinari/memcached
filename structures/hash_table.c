#include "hash_table.h"

#include <stdlib.h>
#include <math.h>

#define RESIZE_FACTOR 5

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, HashFunction hash) {

  // Pedimos memoria para la estructura principal y las casillas.
  HashTable table = malloc(sizeof(struct _HashTable));
  table->elems = malloc(sizeof(struct _DoubleLinkedList) * size);
  table->numElems = 0;
  table->size = size;
  table->comp = comp;
  table->destr = destr;
  table->hash = hash;

  // Inicializamos las casillas con datos nulos.
  for (unsigned idx = 0; idx < size; ++idx) {
    table->elems[idx] = dll_create();
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
unsigned hashtable_capacity(HashTable table) { return table->size; }

/**
 * Destruye la tabla.
 */
void hashtable_destroy(HashTable table) {

  // Destruir cada uno de los datos.
  for (unsigned idx = 0; idx < table->size; ++idx)
    if (table->elems[idx].data != NULL){
      table->destr(table->elems[idx]);
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
void *hashtable_search(HashTable table, void *data) {
  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = table->hash(data) % table->size;
  return dll_search(table->elems[idx], table->comp);
}

/*
 * primo_mas_cercano: unsigned -> unsigned
 * Dado un numero entero positivo, busca el numero primo mayor, mas cercano.
 */
unsigned closest_prime(unsigned n){

unsigned i,j;
  for (i = n; true; i++) {
    if (i % 2 == 0) //si es par no hace nada
      continue;
    for (j = 3; j <= sqrt(i); j += 2) {
      if (i % j == 0) //si es impar pregunta si es multiplo de alguno de los
        break;      //impares (menos el 1) hasta su raiz cuadrada
    }
    if (j > sqrt(i))
      return i;
  }
}

/**
 * tablahash_insertar: TablaHash *void -> void
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void hashtable_insert(HashTable table, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr) {

  //agranda la tabla si tiene un 70% ocupado
  /*
  if (((float)hashtable_nelems(table) / (float)hashtable_capacity(table)) > 0.7)
    hashtable_resize(table, RESIZE_FACTOR);
    DESHABILITADA POR AHORA POR EL MEMMAX
  */

  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = table->hash(data) % table->size;

  dll_insert(table->elems[idx], data, comp, destr);
  table->numElems++;
}

/**
 * tablahash_eliminar: TablaHash *void -> void
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void hashtable_delete(HashTable table, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr) {

  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = table->hash(data) % table->size;

  void dll_node_delete(table->elems[idx], data, comp, destr);

}
