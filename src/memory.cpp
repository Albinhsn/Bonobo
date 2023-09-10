

#include "memory.h"
#include "compiler.h"
#include "object.h"
#include "value.h"
#include <cstdlib>

#ifdef DEBUG_LOG_GC
#include "debug.h"
#include <stdio.h>
#endif

#define GC_HEAP_GROW_FACTOR 2

static void freeObject(Obj *object) {
#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void *)object, object->type);
#endif
  switch (object->type) {
  case OBJ_INSTANCE: {
    ObjInstance *instance = (ObjInstance *)object;
    if (instance->fields) {
      FREE_ARRAY(Value, instance->fields, instance->fieldCap);
    }
    FREE(ObjInstance, object);
    break;
  }
  case OBJ_STRUCT: {
    ObjStruct *strukt = (ObjStruct *)object;
    FREE_ARRAY(ObjString *, strukt->fields, strukt->fieldCap);
    FREE(ObjStruct, object);
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    FREE_ARRAY(uint16_t, function->code, function->codeCap);
    FREE_ARRAY(Value, function->constants, function->constCap);
    FREE_ARRAY(uint16_t, function->lines, function->codeCap);
    FREE(ObjFunction, object);
    break;
  }
  case OBJ_NATIVE: {
    FREE(ObjNative, object);
    break;
  }
  case OBJ_STRING: {
    ObjString *string = (ObjString *)object;
    free(string->chars);
    FREE(ObjString, object);
    break;
  }
  case OBJ_MAP: {
    ObjMap *mp = (ObjMap *)object;
    freeTable(&mp->map);
    FREE(ObjMap, object);
    break;
  }
  case OBJ_ARRAY: {
    ObjArray *array = (ObjArray *)object;
    FREE_ARRAY(Value, array->arr, array->arrCap);
    FREE(ObjArray, object);
    break;
  }
  }
}

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
#ifdef DEBUG_STRESS_GC
  if (newSize > oldSize) {
    collectGarbage();
  }
#endif
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }
  void *result = realloc(pointer, newSize);
  if (result == NULL) {
    exit(1);
  }
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

void freeCompiler(Compiler *compiler) {
  free(compiler->locals);
  free(compiler);
}
