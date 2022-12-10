#include "list_with_lru.h"

NodeLL nodell_create(
	void *data,
	List list,
	LRU lru
){
	NodeLL node = (NodeLL) lru->custom_malloc(lru->forwardRef, sizeof (struct _NodeLL), list);
	node->backList = NULL;
	node->nextList = NULL;
	node->backLRU = NULL;
	node->nextLRU = NULL;
	node->data = data;
	return node;
}

List list_create(){
	List list = (List) malloc(sizeof(struct _List));
	if (list) {
		list->front = NULL;
		list->rear = NULL;
	}
	return list;
}

LRU lru_create(
	AllocationFunction custom_malloc,
	DestructiveFunction dest,
	InitDeallocateFunctionLRU preprocessing,
	EndDeallocateFunctionLRU postprocessing,
	OnAddElementLRU on_add_element,
	OnDeleteElementLRU on_delete_element,
	void *forwardRef
){
	LRU lru = (LRU) malloc(sizeof(struct _LRU));
	if (lru) {
		lru->front = NULL;
		lru->rear = NULL;
		lru->custom_malloc = custom_malloc;
		lru->dest = dest;
		lru->preprocessing = preprocessing;
		lru->postprocessing = postprocessing;
		lru->on_add_element = on_add_element;
		lru->on_delete_element = on_delete_element;
		lru->forwardRef;
	}
	return lru;
}

void list_put(
	List list, 
	LRU lru, 
	void *data,
	ComparativeFunction comp
){
	if(list->front == NULL) {
		list->front = nodell_create(data, lru->custom_malloc, list);
		list->rear = list->front;
		list->front->nextLRU = lru->front;
		lru->front = list->front;
		if(lru->rear == NULL) {
			lru->rear = lru->front;
		} else {
			lru->front->nextLRU->backLRU = lru->front;
		}
	} else {
		NodeLL temp = list->front;
		while(temp && !comp(data, temp->data)) {
			temp = temp->nextList;
		}
		if(temp){
			lru->dest(temp->data);
			temp->data = data;
			if(temp != lru->front){
				temp->backLRU->nextLRU = temp->nextLRU;
				if(temp->nextLRU){
					temp->nextLRU->backLRU = temp->backLRU;
				}
				temp->nextLRU = lru->front;
				lru->front->backLRU = temp;
				lru->front = temp;
			}
		} else {
			temp = list->rear;
			list->rear = nodell_create(data, lru->custom_malloc, list);
			list->rear->backList = temp;
			temp->nextList = list->rear;
			list->rear->nextLRU = lru->front;
			lru->front = list->rear;
			lru->front->nextLRU->backLRU = lru->front;
			lru->on_add_element(lru->forwardRef);
		}
	}
}

void* list_delete(
	List list, 
	LRU lru, 
	void *data,
	ComparativeFunction comp
){
	if(list->front != NULL) {
		NodeLL temp = list->front;
		while(temp && !comp(data, temp->data)) {
			temp = temp->nextList;
		}
		if(temp){
			void *returnData = temp->data;
			if(temp->nextList){
				temp->nextList->backList = temp->backList;
			}
			if(temp->backList) {
				temp->backList->nextList = temp->nextList;
			}
			if(temp->nextLRU){
				temp->nextLRU->backLRU = temp->backLRU;
			}
			if(temp->backLRU) {
				temp->backLRU->nextLRU = temp->nextLRU;
			}
			if(temp == lru->front){
				lru->front = temp->nextLRU;
			}
			if(temp == lru->rear){
				lru->rear = temp->backLRU;
			}
			if(temp == list->front){
				list->front = temp->nextList;
			}
			if(temp == list->rear){
				list->rear = temp->backList;
			}
			lru->on_delete_element(lru->forwardRef);
			free(temp);
			return returnData;
		}
	}
	return NULL;
}

void* list_get(
	List list, 
	void *data,
	ComparativeFunction comp
){
	if(list->front != NULL) {
		NodeLL temp = list->front;
		while(temp && !comp(data, temp->data)) {
			temp = temp->nextList;
		}
		if(temp){
			return temp->data;
		}
	}
	return NULL;
}

// TODO: Check if any thread already deallocate the structure
bool lru_deallocate(LRU lru, List currentList){
	if(lru->rear != NULL) {
		int alreadyDeallocated = 0;
		NodeLL temp = lru->rear;
		List currentListDeallocation = NULL;
		while(temp || alreadyDeallocated >= 10) {
			currentListDeallocation = lru->preprocessing(lru->forwardRef, temp->data, currentList);
			lru->dest(temp->data);
			lru->rear = temp->backLRU;

			if(temp->nextList){
				temp->nextList->backList = temp->backList;
			}
			if(temp->backList){
				temp->backList->nextList = temp->nextList;
			}
			if(currentListDeallocation->front == temp){
				currentListDeallocation->front = temp->nextList;
			}
			if(currentListDeallocation->rear == temp){
				currentListDeallocation->rear = temp->backList;
			}

			temp = lru->rear;
			lru->postprocessing(lru->forwardRef, temp->data, currentList);
			lru->on_delete_element(lru->forwardRef);
		}
		if(temp) {
			temp->nextLRU = NULL;
		}
	} else {
		return false;
	}
}

void list_destroy(List list){
	free(list);
}

void lru_destroy(LRU lru){
	NodeLL current = lru->front;
	while (lru->front) {
		lru->dest(current->data);
		lru->front = current->nextLRU;
		free(current);
	}
	free(lru);
}