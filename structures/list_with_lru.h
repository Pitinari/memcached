#ifndef __LISTWITHLRU_H__
#define __LISTWITHLRU_H__

#include <stdbool.h>
#include <stdlib.h>

typedef bool (*ComparativeFunctionNode) (void *data1, void *data2);
/** Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef void (*DestructiveFunctionNode) (void *data);
/** Libera la memoria alocada para el dato */
typedef void *(*AllocationFunctionNode)(size_t size);
/** malloc personalizado */
typedef List (CallbackFunctionLRU)(void *data);
/** malloc personalizado */

struct _NodeListLRU {
    struct _NodeListLRU *backList, *nextList, *backLRU, *nextLRU;
    void *data;
};

typedef struct _NodeListLRU *NodeListLRU;

struct _List {
    NodeListLRU rear, front;
    AllocationFunctionNode custom_malloc;
    ComparativeFunctionNode comp;
    DestructiveFunctionNode dest;
};

struct _LRU {
    NodeListLRU rear, front;
    AllocationFunctionNode custom_malloc;
    DestructiveFunctionNode dest;
};

typedef struct _List *List;
typedef struct _LRU *LRU;

NodeListLRU node_list_lru_create(
    void *data, 
    AllocationFunctionNode custom_malloc);

List list_create(
    AllocationFunctionNode custom_malloc, 
    ComparativeFunctionNode comp,
    DestructiveFunctionNode dest);

LRU lru_create(
    AllocationFunctionNode custom_malloc,
    DestructiveFunctionNode dest
);

void list_put(List list, LRU lru, void *data);

void* list_delete(List list, LRU lru, void *data);

void* list_get(List list, void *data);

bool lru_deallocate(LRU lru, void *data, CallbackFunctionLRU callback);

void list_destroy(List list);

void lru_destroy(LRU lru);

#endif /* __LISTWITHLRU_H__ */
