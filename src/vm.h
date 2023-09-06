#ifndef cpplox_vm_h
#define cpplox_vm_h

#include "object.h"
#include "opcode.h"
#include "scanner.h"
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
  Value *stackTop;
  Value stack[STACK_MAX];
  String *globalKeys;
  Value *globalValues;
  int globalLen;
  int globalCap;
  Obj **objects;
  int objCap;
  int objLen;
} VM;

extern VM *vm;

#endif
