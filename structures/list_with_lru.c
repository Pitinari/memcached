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
	AllocationFunctionNode custom_malloc,
	DestructiveFunctionNode dest,
	InitDeallocateFunctionLRU preprocessing,
	EndDeallocateFunctionLRU postprocessing,
	void *forwardRef
){
	LRU lru = (LRU) malloc(sizeof(struct _LRU));
	if (lru) {
		lru->front = NULL;
		lru->rear = NULL;
		lru->custom_malloc = custom_malloc;
		lru->dest = dest;
		lru->preprocessing;
		lru->postprocessing;
		lru-> forwardRef;
	}
	return lru;
}

void list_put(
	List list, 
	LRU lru, 
	void *data,
	ComparativeFunctionNode comp
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
		}
	}
}

void* list_delete(
	List list, 
	LRU lru, 
	void *data,
	ComparativeFunctionNode comp
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
			free(temp);
			return returnData;
		}
	}
	return NULL;
}

void* list_get(
	List list, 
	void *data,
	ComparativeFunctionNode comp
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
		List currentList = NULL;
		while(temp || alreadyDeallocated >= 10) {
			currentList = lru->preprocessing(lru->forwardRef, temp->data, currentList);
			lru->dest(temp->data);
			lru->rear = temp->backLRU;

			if(temp->nextList){
				temp->nextList->backList = temp->backList;
			}
			if(temp->backList){
				temp->backList->nextList = temp->nextList;
			}
			if(currentList->front == temp){
				currentList->front = temp->nextList;
			}
			if(currentList->rear == temp){
				currentList->rear = temp->backList;
			}

			temp = lru->rear;
			lru->postprocessing(lru->forwardRef, temp->data, currentList);
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
	while (current) {
		lru->dest(current->data);
		current = lru->front->nextLRU;
		free(lru->front);
	}
	free(lru);
}