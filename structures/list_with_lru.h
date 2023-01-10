#ifndef __LISTWITHLRU_H__
#define __LISTWITHLRU_H__

#include <stdbool.h>
#include <stdlib.h>

/* Nodo con informacion a sus vecinos de la lista y de la LRU */
struct _NodeLL {
	struct _NodeLL *backList, *nextList, *backLRU, *nextLRU;
	void *data;
};
/* Nodo con informacion a sus vecinos de la lista y de la LRU */
typedef struct _NodeLL *NodeLL;

/* Lista de nodos con punteros al inicio y al final */
struct _List {
	NodeLL rear, front;
};
/* Lista de nodos con punteros al inicio y al final */
typedef struct _List *List;

/* Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef bool (*ComparativeFunction) (void *data1, void *data2);
/* Libera la memoria alocada para el dato */
typedef void (*DestructiveFunction) (void *data);
/* malloc personalizado */
typedef void *(*AllocationFunction) (void *forwardRef, size_t size, List currentList);
/* preprocessing deallocate */
typedef List (*InitDeallocateFunctionLRU) (void *forwardRef, void *data, List currentList);
/* postprocessing deallocate */
typedef void (*EndDeallocateFunctionLRU) (void *forwardRef, void *data, List currentList);
/* on add element of the LRU */
typedef void (*OnAddElementLRU) (void *forwardRef);
/* on delete element of the LRU */
typedef void (*OnDeleteElementLRU) (void *forwardRef);
/* take lock of LRU */
typedef void (*TakeLRULock) (void *forwardRef);
/* drop lock of LRU */
typedef void (*DropLRULock) (void *forwardRef);

/* Lista donde se guarda la informacion de los nodos */
struct _LRU {
	NodeLL rear, front;
	AllocationFunction custom_malloc;
	DestructiveFunction dest;
	InitDeallocateFunctionLRU preprocessing;
	EndDeallocateFunctionLRU postprocessing;
	OnAddElementLRU on_add_element;
	OnDeleteElementLRU on_delete_element;
	TakeLRULock take_lru_lock;
	DropLRULock drop_lru_lock;
	void *forwardRef;
};
typedef struct _LRU *LRU;

/* Crea un nuevo nodo */
NodeLL nodell_create(void *data, List currentList, LRU lru);

/* Crea una lista */
List list_create();

/* Crea una LRU */
LRU lru_create(AllocationFunction custom_malloc, DestructiveFunction dest,
								InitDeallocateFunctionLRU preprocessing, EndDeallocateFunctionLRU postprocessing,
								OnAddElementLRU on_add_element, OnDeleteElementLRU on_delete_element,
								TakeLRULock take_lru_lock, DropLRULock drop_lru_lock, void *forwardRef);

/* Inserta el dato en la lista, si ya pertenece, lo actualiza */
void list_put(List list, LRU lru, void *data, ComparativeFunction comp);

/* Retorna el dato del nodo cuya comparacion devuelve true con data y lo
elimina de la lista y la LRU */
void* list_take(List list, LRU lru, void *data, ComparativeFunction comp);

/* Retorna el dato del nodo cuya comparacion devuelve true con data */
void* list_get(List list,	void *data,	ComparativeFunction comp);

/* Libera memoria de la LRU retorna true si elimino un nodo y false en
caso contrario*/
bool lru_deallocate(LRU lru, List currentList);

/* Libera la lista sin liberar sus elementos */
void list_destroy(List list);

/* Liberar la memoria de la LRU y de sus elementos */
void lru_destroy(LRU lru);

#endif /* __LISTWITHLRU_H__ */
