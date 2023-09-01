#ifndef cpplox_stack_h
#define cpplox_stack_h

#include "value.h"
#include <stdexcept>

typedef struct Node {
  Value value;
  Node *next;
} Node;

class Stack {
  Node *head;

public:
  int length;
  void remove(int sp) {
    Node *curr = head;
    while (length > sp) {
      length--;
      Node *oldHead = head;
      head = head->next;
      Value value = oldHead->value;
      delete (oldHead);
    }
  }

  Value get(int idx) {
    Node *curr = head;
    for (int i = 0; i < idx; i++) {
      curr = curr->next;
    }
    return curr->value;
  }
  void update(int idx, Value value) {
    Node *curr = head;
    if (idx > 0) {
      for (int i = 0; i < idx; i++) {
        curr = curr->next;
      }
    }
    curr->value = value;
  }
  void init() {
    head = NULL;
    length = 0;
  }
  Value peek() { return head->value; }

  Value pop() {
    length--;
    Node *oldHead = head;
    head = head->next;
    Value value = oldHead->value;
    delete (oldHead);
    return value;
  }
  void push(Value value) {
    Node *node = new Node();
    node->value = value;
    node->next = head;
    head = node;
    length++;
  }
};

#endif
