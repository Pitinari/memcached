#include <pthread.h>
#include "../structures/queue.h"

struct rw_lock{                // lock
  pthread_mutex_t queueLock;   // mutex para modificar la requestsQueue
  struct Queue *requestsQueue; // queue que guarda las requests
  int mode;                    // estado actual del lock: 0 nothing, 1 reading, 2 writing
  int currentReaders;	       // tiene la cantidad de lectores actuales
};

struct request{
  pthread_cond_t *cond; // variable de condicion
  int type;   // 1 read request, 2 write request
};

void rw_lock_init(struct rw_lock *lock);

void lock_writers(struct rw_lock *arrLock);

void lock_readers(struct rw_lock *arrLock);

void unlock_writers(struct rw_lock *arrLock);

void unlock_readers(struct rw_lock *arrLock);
