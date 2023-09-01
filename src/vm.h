#ifndef cpplox_vm_h
#define cpplox_vm_h

#include "chunk.h"
#include "object.h"
#include "stack.h"
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

#define FRAMES_MAX 90

typedef struct {
  ObjFunction *function;
  std::vector<uint8_t> instructions;
  int ip;
  int sp; // Stack pointer
} CallFrame;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(std::string source);

typedef struct FrameNode {
  CallFrame *frame;
  FrameNode *next;
} FrameNode;

class FrameStack {
  FrameNode *head;

public:
  int length;
  void init() {
    head = NULL;
    length = 0;
  }
  CallFrame *peek() { return head->frame; }

  CallFrame *pop() {
    length--;
    FrameNode *oldHead = head;
    CallFrame *frame = oldHead->frame;
    head = head->next;
    delete(oldHead);

    return frame;
  }
  void push(CallFrame *value) {
    FrameNode *node = new FrameNode();
    node->frame = value;
    node->next = head;
    head = node;
    length++;
  }
};

typedef struct {
  FrameStack *frames;
  std::map<std::string, Value> strings;
  std::map<std::string, Value> globals;
  std::vector<Obj *> objects;
  Stack *stack;
} VM;

extern VM *vm;

#endif
