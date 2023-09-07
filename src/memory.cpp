

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
    FREE(ObjInstance, object);
    break;
  }
  case OBJ_STRUCT: {
    ObjStruct *strukt = (ObjStruct *)object;
    FREE_ARRAY(String, strukt->fields, strukt->fieldCap);
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
    FREE(ObjString, object);
    break;
  }
  case OBJ_MAP: {
    ObjMap *mp = (ObjMap *)object;
    FREE_ARRAY(Value, mp->keys, mp->mapCap);
    FREE_ARRAY(Value, mp->values, mp->mapCap);
    FREE(ObjMap, object);
  }
  case OBJ_ARRAY: {
    ObjArray *array = (ObjArray *)object;
    FREE_ARRAY(Value, array->arr, array->arrCap);
    FREE(ObjArray, object);
  }
  }
}

static void markRoots() {
  for (Value *slot = vm->stack; slot < vm->stackTop; slot++) {
    markValue(*slot);
  }
  for (int i = 0; i < vm->globalLen; i++) {
    markValue(vm->globalValues[i]);
    markObject((Obj *)vm->globalKeys[i]);
  }
  markCompilerRoots();
}
void markObject(Obj *object) {
  if (object == NULL)
    return;
  if (object->isMarked)
    return;
#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void *)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  object->isMarked = true;
  if (vm->grayCapacity < vm->grayCount + 1) {
    vm->grayCapacity = GROW_CAPACITY(vm->grayCapacity);
    vm->grayStack =
        (Obj **)realloc(vm->grayStack, sizeof(Obj *) * vm->grayCapacity);
  }

  vm->grayStack[vm->grayCount++] = object;
}
static void markArray(Value *array, int len) {
  for (int i = 0; i < len; i++) {
    markValue(array[i]);
  }
}

void markValue(Value value) {
  if (IS_OBJ((value))) {
    markObject(AS_OBJ(value));
  }
}

static void blackenObject(Obj *object) {

#ifdef DEBUG_LOG_GC
  printf("%p blacken ", (void *)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  switch (object->type) {
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    markObject((Obj *)function->name);
    markArray(function->constants, function->constP);
    break;
  }
  case OBJ_ARRAY: {
    ObjArray *array = (ObjArray *)object;
    markArray(array->arr, array->arrLen);
    break;
  }
  case OBJ_MAP: {
    ObjMap *mp = (ObjMap *)object;
    markArray(mp->keys, mp->mapLen);
    markArray(mp->values, mp->mapLen);
    break;
  }
  case OBJ_STRUCT: {
    ObjStruct *strukt = (ObjStruct *)object;
    // markArray(strukt->fields, strukt->fieldLen);
    markObject((Obj *)strukt->name);
    break;
  }
  case OBJ_NATIVE: {
    ObjNative *native = (ObjNative *)object;
    native->obj.isMarked = true;
    break;
  }

  case OBJ_STRING: {
    ObjString *string = (ObjString *)object;
    string->obj.isMarked = true;
  } break;
  default: {
    break;
  }
  }
}

static void traceReferences() {
  while (vm->grayCount > 0) {
    Obj *object = vm->grayStack[--vm->grayCount];
    blackenObject(object);
  }
}

static void sweep() {
  Obj *previous = NULL;
  Obj *object = vm->objects;
  while (object != NULL) {
    if (object->isMarked) {
      object->isMarked = false;
      previous = object;
      object = object->next;
    } else {
      Obj *unreached = object;
      object = object->next;
      if (previous != NULL) {
        previous->next = object;
      } else {
        vm->objects = object;
      }

      freeObject(unreached);
    }
  }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
  size_t before = vm->bytesAllocated;
#endif
  markRoots();
  printf("Marked roots\n");
  traceReferences();
  printf("Traced references\n");
  sweep();
  vm->nextGC = vm->bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
  printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
         before - vm->bytesAllocated, before, vm->bytesAllocated, vm->nextGC);

#endif
}

void freeObjects(VM *vm) {
  Obj *obj = vm->objects;
  Obj *next = NULL;
  while (obj) {
    next = obj->next;
    freeObject(obj);
    obj = next;
  }
  free(vm->globalKeys);
  free(vm->globalValues);
  free(vm->grayStack);
}

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  vm->bytesAllocated += newSize - oldSize;
#ifdef DEBUG_STRESS_GC
  if (newSize > oldSize) {
    collectGarbage();
  }
#endif
  if (vm->bytesAllocated > vm->nextGC) {
    collectGarbage();
  }
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
