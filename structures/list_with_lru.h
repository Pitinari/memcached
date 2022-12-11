#ifndef __LISTWITHLRU_H__
#define __LISTWITHLRU_H__

#include <stdbool.h>
#include <stdlib.h>

struct _NodeLL {
	struct _NodeLL *backList, *nextList, *backLRU, *nextLRU;
	void *data;
};

typedef struct _NodeLL *NodeLL;

struct _List {
	NodeLL rear, front;
};

typedef struct _List *List;

typedef bool (*ComparativeFunction) (void *data1, void *data2);
/** Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef void (*DestructiveFunction) (void *data);
/** Libera la memoria alocada para el dato */
typedef void *(*AllocationFunction)(void *forwardRef, size_t size, List currentList);
/** malloc personalizado */
typedef List (*InitDeallocateFunctionLRU)(void *forwardRef, void *data, List currentList);
/** preprocessing deallocate */
typedef void (*EndDeallocateFunctionLRU)(void *forwardRef, void *data, List currentList);
/** postprocessing deallocate */
typedef void (*OnAddElementLRU)(void *forwardRef);
/** on add element of the LRU */
typedef void (*OnDeleteElementLRU)(void *forwardRef);
/** on delete elemtent of the LRU */

struct _LRU {
	NodeLL rear, front;
	AllocationFunction custom_malloc;
	DestructiveFunction dest;
	InitDeallocateFunctionLRU preprocessing;
	EndDeallocateFunctionLRU postprocessing;
	OnAddElementLRU on_add_element;
	OnDeleteElementLRU on_delete_element;
	void *forwardRef;
};

typedef struct _LRU *LRU;

NodeLL nodell_create(
	void *data, 
	List currentList,
	LRU lru);

List list_create();

LRU lru_create(
	AllocationFunction custom_malloc,
	DestructiveFunction dest,
	InitDeallocateFunctionLRU preprocessing,
	EndDeallocateFunctionLRU postprocessing,
	OnAddElementLRU on_add_element,
	OnDeleteElementLRU on_delete_element,
	void *forwardRef
);

void list_put(
	List list, 
	LRU lru, 
	void *data,
	ComparativeFunction comp
);

void* list_take(
	List list, 
	LRU lru, 
	void *data,
	ComparativeFunction comp
);

void* list_get(
	List list, 
	void *data,
	ComparativeFunction comp    
);

bool lru_deallocate(LRU lru, List currentList);

void list_destroy(List list);

void lru_destroy(LRU lru);

#endif /* __LISTWITHLRU_H__ */
