#ifndef __LISTWITHLRU_H__
#define __LISTWITHLRU_H__

#include <stdbool.h>
#include <stdlib.h>

typedef bool (*ComparativeFunctionNode) (void *data1, void *data2);
/** Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef void (*DestructiveFunctionNode) (void *data);
/** Libera la memoria alocada para el dato */
typedef void *(*AllocationFunctionNode)(void *forwardRef, size_t size, List currentList);
/** malloc personalizado */
typedef struct _List *(*InitDeallocateFunctionLRU)(void *forwardRef, void *data, List currentList);
/** preprocessing deallocate */
typedef void (*EndDeallocateFunctionLRU)(void *forwardRef, void *data, List currentList);
/** postprocessing deallocate */

struct _NodeLL {
	struct _NodeLL *backList, *nextList, *backLRU, *nextLRU;
	void *data;
};

typedef struct _NodeLL *NodeLL;

struct _List {
	NodeLL rear, front;
};

typedef struct _List *List;

struct _LRU {
	NodeLL rear, front;
	AllocationFunctionNode custom_malloc;
	DestructiveFunctionNode dest;
	InitDeallocateFunctionLRU preprocessing;
	EndDeallocateFunctionLRU postprocessing;
	void *forwardRef;
};

typedef struct _LRU *LRU;

NodeLL nodell_create(
	void *data, 
	List currentList,
	LRU lru);

List list_create(AllocationFunctionNode custom_malloc);

LRU lru_create(
	AllocationFunctionNode custom_malloc,
	DestructiveFunctionNode dest,
	InitDeallocateFunctionLRU preprocessing,
	EndDeallocateFunctionLRU postprocessing,
	void *forwardRef
);

void list_put(
	List list, 
	LRU lru, 
	void *data,
	AllocationFunctionNode custom_malloc,
	ComparativeFunctionNode comp,
	DestructiveFunctionNode dest
);

void* list_delete(
	List list, 
	LRU lru, 
	void *data,
	ComparativeFunctionNode comp
);

void* list_get(
	List list, 
	void *data,
	ComparativeFunctionNode comp    
);

bool lru_deallocate(LRU lru, List currentList);

void list_destroy(List list);

void lru_destroy(LRU lru);

#endif /* __LISTWITHLRU_H__ */
