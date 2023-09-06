

#include "memory.h"
#include "object.h"
#include "value.h"
#include <cstdlib>

void freeObjects(VM *vm) {
  for (int i = 0; i < vm->objLen; i++) {
    if (vm->objects[i] == NULL) {
      printf("ALREADY FREED\n");
      continue;
    }
    switch (vm->objects[i]->type) {
    case OBJ_STRING: {
      ObjString *string = (ObjString *)vm->objects[i];
      free(string);
      break;
    }
    case OBJ_NATIVE: {
      ObjNative *native = (ObjNative *)vm->objects[i];
      free(native);
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction *fn = (ObjFunction *)vm->objects[i];
      free(fn->constants);
      free(fn->lines);
      free(fn->code);
      free(fn);
      break;
    }
    default: {
      printf("Don't know how to free %d\n", (int)vm->objects[i]->type);
      exit(1);
    }
    }
  }
  free(vm->objects);
  free(vm->globalKeys);
  free(vm->globalValues);
}

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void *result = realloc(pointer, newSize);
  if (result == NULL)
    exit(1);
  return result;
}

void freeParser(Parser *parser) {
  if (parser->current) {

    free(parser->current);
  }
  if (parser->previous) {
    free(parser->previous);
  }

  free(parser);
};

void freeScanner(Scanner *scanner) { free(scanner); }

Value *freeFrame(VM *vm) {
  CallFrame *f2 = vm->frames[vm->fp - 1];
  vm->fp--;
  Value *sp = f2->sp;
  free(f2->sp);
  free(f2);
  return sp;
};
void freeCompiler(Compiler *compiler) {
  free(compiler->locals);
  free(compiler); 
}
