#ifndef __DOUBLELINKEDLIST_H__
#define __DOUBLELINKEDLIST_H__

#include <stdbool.h>
#include <stdlib.h>

typedef bool (*ComparativeFunctionDLL) (void *data1, void *data2);
/** Retorna un booleano que es true si los datos son iguales y false en caso
contrario */
typedef void (*DestructiveFunctionDLL) (void *data);
/** Libera la memoria alocada para el dato */
typedef void *(*AlloccateFunctionDLL)(size_t size);
/** malloc personalizado */

struct _NodeDLL {
    struct _Node *back, *next;
    void *data;
};

typedef struct _NodeDLL *NodeDLL;

struct _DoubleLinkedList {
    struct NodeDLL rear, front;
    AlloccateFunctionDLL custom_malloc;
};

typedef struct _DoubleLinkedList *DoubleLinkedList;

NodeDLL node_dll_create(void *data);

DoubleLinkedList dll_create(AllocateFunctionDLL custom_malloc);

void dll_push(DoubleLinkedList dll, void *data);

void* dll_pop(DoubleLinkedList dll);

void* dll_search(DoubleLinkedList dll, void *key, ComparativeFunctionDLL comp);

void dll_insert(DoubleLinkedList dll, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr);

void dll_node_delete(DoubleLinkedList dll, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr);

void dll_destroy(DoubleLinkedList dll, DestructiveFunctionDLL destr);

#endif /* __DOUBLELINKEDLIST_H__ */
