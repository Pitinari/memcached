#include "hash_table.h"

#include <stdlib.h>
#include <math.h>

#define RESIZE_FACTOR 5

/**
 * Crea una nueva tabla hash vacia, con la capacidad dada.
 */
HashTable create_hashtable(unsigned size, ComparativeFunction comp,
                         DestructiveFunction destr, HashFunction hash,
                         CopyFunction copy) {

  // Pedimos memoria para la estructura principal y las casillas.
  HashTable table = malloc(sizeof(struct _HashTable));
  table->elems = malloc(sizeof(struct HashNode) * size);
  table->numElems = 0;
  table->size = size;
  table->copy = copy;
  table->comp = comp;
  table->destr = destr;
  table->hash = hash;

  // Inicializamos las casillas con datos nulos.
  for (unsigned idx = 0; idx < size; ++idx) {
    table->elems[idx].data = NULL;
    table->elems[idx].eliminated = false;
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
      table->destr(table->elems[idx].data);
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
  unsigned idx, numCollisions = 0;
  idx = table->hash(data,numCollisions) % table->size;

  // Retornar NULL si la casilla estaba vacia.
  if (table->elems[idx].data == NULL)
    return NULL;
  // Retornar el dato de la casilla si hay concidencia y no esta anteriormente eliminado.
  else if (table->comp(table->elems[idx].data, data) && !(table->elems[idx].eliminated))
    return table->elems[idx].data;
  // En otro caso hay una colision
  else{
    //comenzamos a buscar nuevos hash para el dato, hasta que 
    //haya coincidencia
    idx = table->hash(data,++numCollisions) % table->size;
    while (table->elems[idx].data != NULL){
      if (table->comp(table->elems[idx].data, data)){
        if (!(table->elems[idx].eliminated))
          //coincidencia y no eliminado
          return table->elems[idx].data;
        else
          //coincidencia pero eliminado
          return NULL;
      }
      idx = table->hash(data,++numCollisions) % table->size;
    }
    // si sale del while por una casilla nula, entonces no esta en la tabla
    return NULL;
  }
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

/*
 * tablahash_agrandar: TablaHash -> void
 * Dado un numero entero positivo, busca el numero primo mayor, mas cercano.
 */
void hashtable_resize (HashTable oldTable, int resizeFactor){
  //busca un primo veces mas grande, crea un array de casillas de ese tamano y guarda el array viejo
  unsigned oldCapacity = hashtable_capacity(oldTable), newCapacity = closest_prime(hashtable_capacity(oldTable) * resizeFactor);
  struct HashNode *previousData = oldTable->elems;
  oldTable->elems = malloc(sizeof(struct HashNode) * newCapacity);
  oldTable->numElems = 0;
  oldTable->size = newCapacity;

  //inicializa el array nuevo
  for (unsigned idx = 0; idx < newCapacity; ++idx) {
    oldTable->elems[idx].data = NULL;
    oldTable->elems[idx].eliminated = false;
  }

  //ingresa todos los elementos del array viejo en el nuevo
  for (unsigned i = 0; i < oldCapacity ; i++){
    if ((previousData[i].data != NULL) && (previousData[i].eliminated == false)){
      hashtable_insert(oldTable, previousData[i].data);
    }
    oldTable->destr(previousData[i].data); //libera el dato del array viejo
  }
  free(previousData); //libera el array viejo
  return;
}

/**
 * tablahash_insertar: TablaHash *void -> void
 * Inserta un dato en la tabla, o lo reemplaza si ya se encontraba.
 */
void hashtable_insert(HashTable table, void *data) {

  //agranda la tabla si tiene un 70% ocupado
  if (((float)hashtable_nelems(table) / (float)hashtable_capacity(table)) > 0.7)
    hashtable_resize(table, RESIZE_FACTOR);
  
  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned numCollisions = 0, idx;
  idx = table->hash(data,numCollisions) % table->size;
  
  // Insertar el dato si la casilla estaba libre.
  if (table->elems[idx].data == NULL) {
    table->numElems++;
    table->elems[idx].data = table->copy(data);
    return;
  }
  // busca si el dato ya esta en la tabla
  else if (hashtable_search (table, data) == NULL){
    // en caso de no estarlo, itera hasta encontrar una casilla
    // libre o eliminada
    while(table->elems[idx].data){
      if(table->elems[idx].eliminated){
        table->destr(table->elems[idx].data);
        table->elems[idx].data = table->copy(data);
        table->elems[idx].eliminated = false;
        table->numElems++;
        return;
      }
      idx = table->hash(data,++numCollisions) % table->size;
    }
    table->elems[idx].data = table->copy(data);
    table->numElems++;
    return;
  }
  else{ //sino itera hasta encontrar al contacto
    while(!table->comp(table->elems[idx].data,data))
      idx = table->hash(data,++numCollisions) % table->size;

    table->destr(table->elems[idx].data);
    table->elems[idx].data = table->copy(data);
    return;
  }

}

/**
 * tablahash_eliminar: TablaHash *void -> void
 * Elimina el dato de la tabla que coincida con el dato dado.
 */
void hashtable_delete(HashTable table, void *data) {

  // Calculamos la posicion del dato dado, de acuerdo a la funcion hash.
  unsigned idx, numCollisions = 0;
  idx = table->hash(data, numCollisions) % table->size;

  // Retornar NULL si la casilla estaba vacia.
  if (table->elems[idx].data == NULL)
    return;
  // Retornar el dato de la casilla si hay concidencia.
  else if (table->comp(table->elems[idx].data, data)){
    table->elems[idx].eliminated = true;
    table->numElems--;
    return;
  }
  // iterar mientras las casillas sean ocupadas
  while (table->elems[idx].data){
    if (table->comp(table->elems[idx].data, data)){
      //si hay coincidencia y no esta eliminado
      if (table->elems[idx].eliminated == false){
        table->elems[idx].eliminated = true;
        table->numElems--;
        //lo elimina y resta del numero de elementos
      }
      return;
    }
    idx = table->hash(data,++numCollisions) % table->size;
  }
  return; //llego a una casilla NULL
}

// tablahash_a_lista: TablaHash -> *void
// Dada una tabla hash y una lista de contactos del tamano de la cantidad
// de posiciones ocupadas en la tabla hash, hace que todos los punteros de 
// la lista apunten a los elementos de la tabla
void *hashtable_to_list (HashTable table){
  unsigned j = 0;
  void **list = malloc(sizeof(void*)*table->numElems);
  for (unsigned i = 0; i < table->size ; i++){
    if(table->elems[i].data == NULL || table->elems[i].eliminated == true)
      continue;
    list[j++] = table->elems[i].data;
  }
  return list;
}