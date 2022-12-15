#include "list_with_lru.h"

NodeLL nodell_create(void *data, List list, LRU lru) {
	NodeLL node = (NodeLL) lru->custom_malloc(lru->forwardRef, sizeof (struct _NodeLL), list);
	/* Si se consiguio memoria */
	if (node != NULL) {
		node->backList = NULL;
		node->nextList = NULL;
		node->backLRU = NULL;
		node->nextLRU = NULL;
		node->data = data;
	}
	return node;
}

List list_create() {
	List list = (List) malloc(sizeof(struct _List));
	/* Si se consiguio memoria */
	if (list != NULL) {
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
) {
	LRU lru = (LRU) malloc(sizeof(struct _LRU));
	/* Si se consiguio memoria */
	if (lru != NULL) {
		lru->front = NULL;
		lru->rear = NULL;
		lru->custom_malloc = custom_malloc;
		lru->dest = dest;

		// Estas son funciones a llamar antes de borrar un nodo con lru_deallocate
		// Sirven, por ejemplo, para tomar locks y checkear a que lista pertenece un nodo
		lru->preprocessing = preprocessing;
		lru->postprocessing = postprocessing;

		// Callbacks para llamar cuando se agregue y borre un elemento, respectivamente
		lru->on_add_element = on_add_element;
		lru->on_delete_element = on_delete_element;

		// Es un puntero de referencia a la estructura que implemente estas listas con lru
		// Esto se hizo con la intencion de generalizar la estructura que lo implemente
		lru->forwardRef = forwardRef;
	}
	return lru;
}

void list_put(List list, LRU lru, void *data, ComparativeFunction comp) {
	/* Caso lista vacia */
	if (list == NULL || lru == NULL) return;
	if (list->front == NULL) {
		list->front = nodell_create(data, list, lru);
		if (list->front == NULL) return; /* No se pudo allocar */
		list->rear = list->front;
		list->front->nextLRU = lru->front;
		lru->front = list->front;
		/* Caso LRU vacia*/
		if (lru->rear == NULL) {
			lru->rear = lru->front;
		} 
		else {
			lru->front->nextLRU->backLRU = lru->front;
		}
		lru->on_add_element(lru->forwardRef);
	} 
	else {
		/* Buscamos si el elemento ya pertenece a la lista */
		NodeLL temp = list->front;
		while (temp && !comp(data, temp->data)) {
			temp = temp->nextList;
		}
		/* Si fue encontrado, actualizamos el dato */
		if (temp != NULL) {
			lru->dest(temp->data);
			temp->data = data;
			if (temp != lru->front) {
				temp->backLRU->nextLRU = temp->nextLRU;
				if (temp->nextLRU) {
					temp->nextLRU->backLRU = temp->backLRU;
				}
				temp->nextLRU = lru->front;
				lru->front->backLRU = temp;
				lru->front = temp;
			}
		}
		/* Si no fue encontrado se guarda en el final de la lista*/
		else {
			temp = list->rear;
			list->rear = nodell_create(data, list, lru);
			if (list->rear == NULL) return; /* No se pudo allocar */
			list->rear->backList = temp;
			temp->nextList = list->rear;
			list->rear->nextLRU = lru->front;
			lru->front = list->rear;
			lru->front->nextLRU->backLRU = lru->front;
			lru->on_add_element(lru->forwardRef);
		}
	}
}

void* list_take(List list, LRU lru, void *data, ComparativeFunction comp) {
	if (list == NULL || lru == NULL) return NULL;
	if (list->front != NULL) {
		/* Buscamos el nodo deseado*/
		NodeLL temp = list->front;
		while (temp && !comp(data, temp->data)) {
			temp = temp->nextList;
		}
		if (temp != NULL) {
			/* Acomodamos los punteros de los vecinos */
			void *returnData = temp->data;
			if (temp->nextList) {
				temp->nextList->backList = temp->backList;
			}
			if (temp->backList) {
				temp->backList->nextList = temp->nextList;
			}
			if (temp->nextLRU) {
				temp->nextLRU->backLRU = temp->backLRU;
			}
			if (temp->backLRU) {
				temp->backLRU->nextLRU = temp->nextLRU;
			}
			/* Acomodamos los punteros de las listas */
			if (temp == lru->front) {
				lru->front = temp->nextLRU;
			}
			if (temp == lru->rear) {
				lru->rear = temp->backLRU;
			}
			if (temp == list->front) {
				list->front = temp->nextList;
			}
			if (temp == list->rear) {
				list->rear = temp->backList;
			}
			/* Eliminamos el nodo y devolvemos el dato guardado */
			lru->on_delete_element(lru->forwardRef);
			free(temp);
			return returnData;
		}
	}
	return NULL;
}

void* list_get(List list, void *data, ComparativeFunction comp) {
	if (list->front != NULL) {
		/* Buscamos el nodo deseado */
		NodeLL temp = list->front;
		while (temp != NULL && !comp(data, temp->data)) {
			temp = temp->nextList;
		}
		/* Lo retornamos si fue encontrado */
		if (temp != NULL) {
			return temp->data;
		}
	}
	return NULL;
}

// Funcion para borrar elementos desde la LRU
bool lru_deallocate(LRU lru, List currentList) {
	if (lru == NULL) return false;
	if (lru->rear != NULL) {
		int alreadyDeallocated = 0;
		// Iniciamos borrando desde el ultimo nodo agregado
		NodeLL temp = lru->rear;
		List currentListDeallocation = NULL;
		// Hasta borrar 10 o que las listas esten vacias
		while(temp != NULL || alreadyDeallocated < 10) {
			// preprocessing es para hacer los procedimientos previos a empezar a borrar un nodo
			// currentList es para un caso borde de si deallocate se llamo mientras se inserta en la lista
			currentListDeallocation = lru->preprocessing(lru->forwardRef, temp->data, currentList);
			// Destruimos el nodo y acomodamos la lru y lista
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
			// Luego, de forma similar a preprocessing, se llama a postprocessing
			lru->postprocessing(lru->forwardRef, temp->data, currentList);
			// Finalmente aplicamos la callback de borrar un elementos de una lista
			lru->on_delete_element(lru->forwardRef);
		}
		if (temp != NULL) {
			temp->nextLRU = NULL;
		}
		return true;
	} 
	else {
		return false;
	}
}

void list_destroy(List list) {
	free(list);
}

void lru_destroy(LRU lru) {
	if (lru == NULL) return;
	NodeLL current = lru->front;
	while (lru->front) {
		lru->dest(current->data);
		lru->front = current->nextLRU;
		free(current);
	}
	free(lru);
}