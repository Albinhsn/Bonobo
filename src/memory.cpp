

#include "memory.h"
#include <cstdlib>

void freeObjects(VM *vm) {
  for (int i = 0; i < vm->op; i++) {
    if (vm->objects[i] == NULL) {
      printf("ALREADY FREED\n");
      continue;
    }
    switch (vm->objects[i]->type) {
    case OBJ_STRING: {
      ObjString *string = (ObjString *)vm->objects[i];
      delete (string);
      break;
    }
    case OBJ_NATIVE: {
      ObjNative *native = (ObjNative *)vm->objects[i];
      delete (native);
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction *fn = (ObjFunction *)vm->objects[i];
      delete (fn);
      break;
    }
    default: {
      printf("Don't know how to free %d\n", (int)vm->objects[i]->type);
      exit(1);
    }
    }
  }
}

void freeParser(Parser *parser) {
  if (parser->current) {

    delete (parser->current);
  }
  if (parser->previous) {
    delete (parser->previous);
  }

  delete (parser);
};

void freeScanner(Scanner *scanner) { delete (scanner); }

Value *freeFrame(VM *vm) {
  CallFrame *f2 = vm->frames[vm->fp - 1];
  vm->fp--;
  Value *sp = f2->sp;
  delete (f2);
  return sp;
};
void freeCompiler(Compiler *compiler) { delete (compiler); }
