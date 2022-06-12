#include <stdio.h>
#include <stdlib.h>

struct QNode {
    void *key;
    struct QNode* next;
};

struct Queue {
    struct QNode *front, *rear;
};

struct QNode* newNode(void *k);

struct Queue* createQueue();

void enQueue(struct Queue* q, void *k);

void *deQueue(struct Queue* q);

int emptyQueue(struct Queue* q);

void *firstQueue(struct Queue* q);