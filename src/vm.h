#ifndef cpplox_vm_h
#define cpplox_vm_h

#include "chunk.h"
#include "object.h"
#include <map>
#include <stdexcept>
#include <string>
#include <vector>

#define FRAMES_MAX 90
#define STACK_MAX 255

typedef struct CallFrame {
  ObjFunction *function;
  std::vector<uint8_t> instructions;
  int ip;
  Value *sp; // Stack pointer
  CallFrame(ObjFunction *f, Value *s)
      : function(f), instructions(f->chunk->code), ip(0), sp(s){};
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
  Value *stackTop;
  Value stack[STACK_MAX];
  std::map<std::string, Value> strings;
  std::map<std::string, Value> globals;
  std::vector<Obj *> objects;
  VM()
      : frames(new FrameStack), strings(std::map<std::string, Value>()),
        globals(std::map<std::string, Value>()),
        objects(std::vector<Obj *>()){};
} VM;

extern VM *vm;

#endif
