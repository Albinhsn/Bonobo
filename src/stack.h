#ifndef cpplox_stack_h
#define cpplox_stack_h

#include "value.h"
#include <stdexcept>

typedef struct Node {
  Value value;
  Node *next;
  Node(Value v, Node *n) : value(v), next(n){};
} Node;

class Stack {
  Node *head;

public:
  Stack() {
    head = NULL;
    length = 0;
  }
  int length;
  void remove(int sp) {
    Node *curr = head;
    while (length > sp) {
      length--;
      Node *oldHead = head;
      head = head->next;
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
    Node *node = new Node(value, head);
    head = node;
    length++;
  }
};

#endif
