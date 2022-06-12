#include <stdlib.h>
#include <pthread.h>
#include "rwlock.h"

// Inicializador de variables
void rw_lock_init (struct rw_lock *arrLock) {
  pthread_mutex_init(&arrLock->queueLock, NULL);
  arrLock->requestsQueue = create_queue();
  arrLock->mode = 0;
  arrLock->currentReaders = 0;
}

// LECTORES

// Llamada del lock para los lectores
void lock_readers(struct rw_lock *arrLock) {
  // pide el lock de las variables necesarias para pedir el lock de lectura
  pthread_mutex_lock(&arrLock->queueLock);
  if (arrLock->mode == 0) { // si el modo actual esta undefined (nadie tiene el lock)
    arrLock->mode = 1;  // pone el modo en lectura
    arrLock->currentReaders = 1;  // setea la cantidad de lectores en 1
    pthread_mutex_unlock(&arrLock->queueLock);
    return;
  }
  if (arrLock->mode == 1 && empty_queue(arrLock->requestsQueue)) { // si estan leyendo y nadie esta esperando
	  arrLock->currentReaders++;  // se agrega en la cantidad de lectores
	  pthread_mutex_unlock(&arrLock->queueLock);
    return;
  }
  // crea una request de lock
  struct request* req = malloc(sizeof(struct request));
  pthread_cond_t* cond = malloc(sizeof(pthread_cond_t));
  pthread_cond_init(cond, NULL);
  req->cond = cond;
  req->type = 1;
  push_queue(arrLock->requestsQueue, (void *)req); // pushea la request a la queue

  pthread_cond_wait(cond, &arrLock->queueLock); // espera a que el siguiente en la cola de espera lo levante
  // se levanta
  free(cond);
  struct request *first = first_queue(arrLock->requestsQueue);  // toma el siguiente en la queue
  if (first != NULL) {
    if (first->type == 1){
      first = pop_queue(arrLock->requestsQueue);
      arrLock->currentReaders++;
      pthread_cond_signal(first->cond);
      // si el siguiente es lector se saca de la cola, se agrega a la cantidad de lectores y se despierta
      free(first);
    }
  }
  pthread_mutex_unlock(&arrLock->queueLock);
  return;
}

// llamada al unlock para los lectores
void unlock_readers(struct rw_lock *arrLock) {
  pthread_mutex_lock(&arrLock->queueLock);
  arrLock->currentReaders--;  // resta la cantidad de lectores
  if(arrLock->currentReaders != 0){ // si hay mas lectores sale
  	pthread_mutex_unlock(&arrLock->queueLock);
	  return;
  }
  // si no hay nadie mas leyendo toma el siguiente en la queue
  struct request *req = (struct request*) pop_queue(arrLock->requestsQueue);
  if (req == NULL) {
    arrLock->mode = 0; // si no hay nadie pone el estado en undefined
  }
  else {
    pthread_cond_signal(req->cond); // si hay alguien lo despierta
    arrLock->mode = req->type;  // y pone el estado en el modo indicado
    free(req);
  }
  pthread_mutex_unlock(&arrLock->queueLock);
  return;
}


// ESCRITORES

// llamada al lock para los escritores
void lock_writers(struct rw_lock *arrLock) {
  pthread_mutex_lock(&arrLock->queueLock);
  if (arrLock->mode == 0) { // si el modo actual esta undefined (nadie tiene el lock)
    arrLock->mode = 2;  // pone el modo en escritura
    pthread_mutex_unlock(&arrLock->queueLock);
    return;
  }
  // crea una request de lock
  struct request* req = malloc(sizeof(struct request));
  pthread_cond_t* cond = malloc(sizeof(pthread_cond_t));
  pthread_cond_init(cond, NULL);
  req->cond = cond;
  req->type = 2;
  push_queue(arrLock->requestsQueue, (void *)req);  // pushea la request a la queue

  pthread_cond_wait(cond, &arrLock->queueLock); // espera a que el siguiente en la cola de espera lo levante
  free(cond);
  pthread_mutex_unlock(&arrLock->queueLock);

  return;
}

// llamada al unlock para los escritores
void unlock_writers(struct rw_lock *arrLock) {
  pthread_mutex_lock(&arrLock->queueLock);
  // toma el siguiente esperando en la cola
  struct request *req = pop_queue(arrLock->requestsQueue);
  if (req == NULL) {
    arrLock->mode = 0; // si no hay nadie esperado, pone el estado en undefined
  }
  else {  // en caso de haber alguien esperando
    if(req->type == 1) arrLock->currentReaders++; // si es lector agrega uno a la cantidad de lectores leyendo
    pthread_cond_signal(req->cond); // lo despierta
    arrLock->mode = req->type;  // pone el estado del lock en el del tipo de la request
    free(req);
  }
  pthread_mutex_unlock(&arrLock->queueLock);
  return;
}