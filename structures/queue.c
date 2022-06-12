#include <stdio.h>
#include <stdlib.h>
#include "queue.h"

struct QNode* new_node(void *k) {
    struct QNode* temp = (struct QNode*)malloc(sizeof(struct QNode));
    temp->key = k;
    temp->next = NULL;
    return temp;
}

struct Queue* create_queue() {
    struct Queue* q = (struct Queue*)malloc(sizeof(struct Queue));
    q->front = q->rear = NULL;
    return q;
}

void push_queue(struct Queue* q, void *k) {
  struct QNode* temp = new_node(k);

  if (q->rear == NULL) {
    q->front = q->rear = temp;
    return;
  }

  q->rear->next = temp;
  q->rear = temp;
}

void *pop_queue(struct Queue* q) {
  if (q->front == NULL)
    return NULL;

  struct QNode* temp = q->front;

  q->front = q->front->next;

  if (q->front == NULL)
    q->rear = NULL;
  void* ret = temp->key;
  free(temp);
  return ret;
}

int empty_queue(struct Queue* q) {
  return (q->front == NULL);
}

void *first_queue(struct Queue* q) {
  if (q->front == NULL) return NULL;
  return q->front->key;
}