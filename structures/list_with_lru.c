#include "list_with_lru.h"

NodeListLRU node_list_lru_create(
    void *data, 
    AllocationFunctionNode custom_malloc
){
    NodeListLRU node = (NodeListLRU) custom_malloc(sizeof(struct _NodeListLRU));
    node->backList = NULL;
    node->nextList = NULL;
    node->backLRU = NULL;
    node->nextLRU = NULL;
    node->data = data;
    return node;
}

List list_create(
    AllocationFunctionNode custom_malloc, 
    ComparativeFunctionNode comp,
    DestructiveFunctionNode dest
){
    List list = (List) malloc(sizeof(struct _List));
    listWithLRU->front = NULL;
    listWithLRU->rear = NULL;
    listWithLRU->custom_malloc = custom_malloc;
    return listWithLRU;
}

void list_with_lru_push(ListWithLRU listWithLRU, void *data){
    NodeListLRU temp = listWithLRU->front;
    listWithLRU->front = node_dll_create(data, listWithLRU->custom_malloc);
    if(temp == NULL){ //dll vacia
        listWithLRU->rear = listWithLRU->front;
        return;
    }
    ((NodeListLRU)listWithLRU->front)->next = temp;    // Nuevo -> Antiguo
    (NodeListLRU)temp->back = (NodeListLRU)listWithLRU->front;    // Antiguo -> Nuevo
    return;
}

void* list_with_lru_pop(ListWithLRU listWithLRU){
    if(listWithLRU->rear == NULL){
        return NULL;
    }
    void *data = listWithLRU->rear->data;
    NodeListLRU temp = listWithLRU->rear;
    if(temp->back == NULL){
        listWithLRU->front = listWithLRU->rear = NULL;
    }
    else {
        (NodeListLRU)listWithLRU->rear = (NodeListLRU)temp->back;
        listWithLRU->rear->next = NULL;
    }
    free(temp);
    return data;
}

void* list_with_lru_search(ListWithLRU listWithLRU, void *key, ComparativeFunctionNode comp){
    NodeListLRU node = listWithLRU->front;
    while(node){
        if(comp(node->data, key))
            return node->data;
        node = (NodeListLRU)node->next;
    }
    return NULL;
}

void list_with_lru_insert(ListWithLRU listWithLRU, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr){
    NodeDLL node = listWithLRU->front;
    while(node){
        if(comp(node->data, data)){
            destr(node->data);
            node->data = data;
            return;
        }
        node = (NodeDLL)node->next;
    }
    node = node_dll_create(data, listWithLRU->custom_malloc);
    node->back = listWithLRU->rear;
    listWithLRU->rear = listWithLRU->rear->next = node;
    return;
}

void list_with_lru_node_delete(ListWithLRU listWithLRU, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr){
    NodeDLL node = listWithLRU->front;
    while(node){
        if(comp(node->data, data)){
            destr(node->data);
            if(node->back == NULL){
                listWithLRU->front = node->next;
            }
            else {
                ((NodeDLL)node->back)->next = node->next;
            }
            if(node->next == NULL){
                listWithLRU->rear = node->back;
            }
            else {
                ((NodeDLL)node->next)->back = node->back;
            }
            free(node);
            break;
        }
        node = node->next;
    }
}

void list_with_lru_destroy(ListWithLRU listWithLRU, DestructiveFunctionDLL destr){
    NodeDLL node = listWithLRU->front;
    while(node){
        destr(node->data);
        if(node->next){
            node = node->next;
            free(node->back);
        }
        else{
            free(node);
            node = NULL;
        }
    }
    free(listWithLRU);
}
