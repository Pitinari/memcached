#ifndef __QUEUE_H__
#define __QUEUE_H__

#include <stdio.h>
#include <stdlib.h>

struct QNode {
    void *key;
    struct QNode* next;
};

struct Queue {
    struct QNode *front, *rear;
};

struct QNode* new_node(void *k);

struct Queue* create_queue();

void push_queue(struct Queue* q, void *k);

void *pop_queue(struct Queue* q);

int empty_queue(struct Queue* q);

void *first_queue(struct Queue* q);

#endif /* __QUEUE_H__ */