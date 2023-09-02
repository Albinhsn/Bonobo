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
#define GLOBAL_MAX 120 
#define OBJECT_MAX 150

typedef struct CallFrame {
  ObjFunction *function;
  std::vector<uint8_t> instructions;
  int ip;
  Value *sp; // Stack pointer
} CallFrame;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

void initVM();
void freeVM();
InterpretResult interpret(std::string source);

typedef struct VM {
  int fp;
  CallFrame *frames[FRAMES_MAX];
  Value *stackTop;
  Value stack[STACK_MAX];
  std::string globalKeys[GLOBAL_MAX];
  Value globalValues[GLOBAL_MAX];
  int gp;
  Obj *objects[OBJECT_MAX];
  int op;
} VM;

extern VM *vm;

#endif
