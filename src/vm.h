#ifndef cpplox_vm_h
#define cpplox_vm_h

#include "object.h"
#include "opcode.h"
#include "scanner.h"
#include "table.h"
#include <stdexcept>
#include <string>
#include <vector>

#define FRAMES_MAX 90
#define STACK_MAX 255

typedef struct CallFrame {
  ObjFunction *function;
  uint16_t *instructions;
  int ip;
  Value *sp;
} CallFrame;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(const char *source);

typedef struct VM {
  int fp;
  CallFrame *frames[FRAMES_MAX];
  Value stack[STACK_MAX];
  Value *stackTop;
  Table globals;
  Table strings;
  Obj *objects;
  Obj **grayStack;
  int grayCount;
  int grayCapacity;
  size_t bytesAllocated;
  size_t nextGC;
} VM;

extern VM *vm;

Value popStack();
void pushStack(Value value);

#endif
