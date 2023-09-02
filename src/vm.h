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
  FrameNode(CallFrame *f, FrameNode *n) : frame(f), next(n){};
} FrameNode;

class FrameStack {
  FrameNode *head;

public:
  int length;
  FrameStack() {
    head = NULL;
    length = 0;
  }
  CallFrame *peek() { return head->frame; }

  CallFrame *pop() {
    length--;
    FrameNode *oldHead = head;
    CallFrame *frame = oldHead->frame;
    head = head->next;
    delete (oldHead);

    return frame;
  }
  void push(CallFrame *value) {
    head = new FrameNode(value, head);
    length++;
  }
};

typedef struct VM {
  FrameStack *frames;
  std::map<std::string, Value> strings;
  std::map<std::string, Value> globals;
  std::vector<Obj *> objects;
  Stack *stack;
  VM()
      : frames(new FrameStack), strings(std::map<std::string, Value>()),
        globals(std::map<std::string, Value>()), objects(std::vector<Obj *>()),
        stack(new Stack){};
} VM;

extern VM *vm;

#endif
