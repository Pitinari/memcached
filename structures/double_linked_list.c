#include "double_linked_list"

NodeDLL node_dll_create(void *data, AllocationFunction custom_malloc){
    NodeDLL node = (NodeDLL) custom_malloc(sizeof(struct _NodeDLL));
    node->back = NULL;
    node->next = NULL;
    node->data = data;
    return node;
}

DoubleLinkedList dll_create(AllocationFunction custom_malloc){
    DoubleLinkedList dll = (DoubleLinkedList) custom_malloc(sizeof(struct _DoubleLinkedList));
    dll->front = NULL;
    dll->rear = NULL;
    dll->custom_malloc = custom_malloc;
    return dll;
}

void dll_push(DoubleLinkedList dll, void *data){
    NodeDLL temp = dll->front;
    dll->front = node_dll_create(data, dll->custom_malloc);
    if(temp == NULL){ //dll vacia
        dll->rear = dll->front;
        return;
    }
    dll->front->next = temp;    // Nuevo -> Antiguo
    temp->back = dll->front;    // Antiguo -> Nuevo
    return;
}

void* dll_pop(DoubleLinkedList dll){
    if(dll->rear == NULL){
        return NULL;
    }
    void *data = dll->rear->data;
    NodeDLL temp = dll->rear;
    if(temp->back == NULL){
        dll->front = dll->rear = NULL;
    }
    else {
        dll->rear = temp->back;
        dll->rear->next = NULL;
    }
    free(temp);
    return data;
}

void* dll_search(DoubleLinkedList dll, void *key, ComparativeFunctionDLL comp){
    NodeDLL node = dll->front;
    while(node){
        if(comp(node->data, key))
            return node->data;
        node = node->next;
    }
    return NULL;
}

void dll_insert(DoubleLinkedList dll, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr){
    NodeDLL node = dll->front;
    while(node){
        if(comp(node->data, data)){
            destr(node->data);
            node->data = data;
            return;
        }
        node = node->next;
    }
    node = node_dll_create(data, dll->custom_malloc);
    node->back = dll->rear;
    dll->rear = dll->rear->next = node;
    return;
}

void dll_node_delete(DoubleLinkedList dll, void *data, ComparativeFunctionDLL comp, DestructiveFunctionDLL destr){
    NodeDLL node = dll->front;
    while(node){
        if(comp(node->data, data)){
            destr(node->data);
            if(node->back == NULL){
                dll->front = node->next;
            }
            else {
                node->back->next = node->next;
            }
            if(node->next == NULL){
                dll->rear = node->back;
            }
            else {
                node->next->back = node->back;
            }
            free(node);
        }
        node = node->next;
    }
    return;
}

void dll_destroy(DoubleLinkedList dll, DestructiveFunctionDLL destr){
    NodeDLL node = dll->front;
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
    free(dll);
}
