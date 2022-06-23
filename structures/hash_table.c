#include "hash_table.h"

#include <stdlib.h>
#include <math.h>

#define RESIZE_FACTOR 5

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr) {

  // Pedimos memoria para la estructura principal y las casillas.
  HashTable table = malloc(sizeof(struct _HashTable));
  table->elems = malloc(sizeof(NodeDLL) * size);
  table->numElems = 0;
  table->size = size;
  table->comp = comp;
  table->destr = destr;

  // Inicializamos las casillas con datos nulos.
  for (unsigned idx = 0; idx < size; ++idx) {
    table->elems[idx] = node_dll_create(NULL);
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
  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = hashedValue % table->size;
  NodeDLL node = table->elems[idx];
  if(node->data == NULL){
    return NULL;
  }
  while(node){
    if(table->comp(node->data, data)){
      return node->data;
    }
    node = node->next;
  }
  return NULL;
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
void hashtable_insert(HashTable table, unsigned hashedValue, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr) {

  //agranda la tabla si tiene un 70% ocupado
  /*
  if (((float)hashtable_nelems(table) / (float)hashtable_capacity(table)) > 0.7)
    hashtable_resize(table, RESIZE_FACTOR);
    DESHABILITADA POR AHORA POR EL MEMMAX
  */

  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = hashedValue % table->size;

  dll_insert(table->elems[idx], data, comp, destr);
  table->numElems++;
}

/**
 * tablahash_eliminar: TablaHash *void -> void
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void hashtable_delete(HashTable table, unsigned hashedValue, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr) {

  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx = hashedValue % table->size;

  void dll_node_delete(table->elems[idx], data, comp, destr);

}
