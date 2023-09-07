
#include "object.h"
#include "memory.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type *)allocateObject(sizeof(type), objectType)

void writeChunks(ObjFunction *function, uint16_t byte1, uint16_t byte2,
                 int line) {
  writeChunk(function, byte1, line);
  writeChunk(function, byte2, line);
}

static Obj *allocateObject(size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, size);
  object->type = type;
  // object->isMarked = false;
  object->next = vm->objects;
  object->isMarked = false;

  vm->objects = object;

#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for %d\n", (void *)object, size, type);
#endif

  return object;
}

void writeChunk(ObjFunction *function, uint16_t byte, int line) {
  if (function->codeCap < function->codeP+ 1) {
    int oldCapacity = function->codeCap;
    function->codeCap = GROW_CAPACITY(oldCapacity);
    function->code =
        GROW_ARRAY(uint16_t, function->code, oldCapacity, function->codeCap);
    function->lines =
        GROW_ARRAY(int, function->lines, oldCapacity, function->codeCap);
  }
  function->code[function->codeP] = byte;
  function->lines[function->codeP] = line;
  function->codeP++;
}

int addConstant(ObjFunction *function, Value value) {
  pushStack(value);
  if (function->constCap < function->constP + 1) {
    int oldCapacity = function->constCap;
    function->constCap = GROW_CAPACITY(oldCapacity);
    function->constants =
        GROW_ARRAY(Value, function->constants, oldCapacity, function->constCap);
  }

  function->constants[function->constP++] = popStack();
  return function->constP - 1;
}

ObjFunction *newFunction() {
  ObjFunction *function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = function->constP = function->constCap = function->codeP  = function->codeCap = 0;
  function->code = NULL;
  function->lines = NULL;
  function->constants = NULL;

  return function;
}

ObjMap *newMap() {
  ObjMap *mp = ALLOCATE_OBJ(ObjMap, OBJ_MAP);
  initTable(&mp->map);

  return mp;
}

ObjArray *newArray(int len) {
  ObjArray *array = ALLOCATE_OBJ(ObjArray, OBJ_ARRAY);
  array->arrLen = len;

  return array;
}

ObjStruct *newStruct(ObjString *name) {
  ObjStruct *strukt = ALLOCATE_OBJ(ObjStruct, OBJ_STRUCT);
  strukt->name = name;
  strukt->fieldCap = 0;
  strukt->fields = NULL;
  strukt->fieldLen = 0;

  return strukt;
}

ObjInstance *newInstance(ObjStruct *strukt) {
  ObjInstance *instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
  instance->fieldLen = 0;
  instance->fieldCap = strukt->fieldCap;
  instance->name = strukt->name;
  instance->strukt = strukt;
  pushStack(OBJ_VAL(instance));
  instance->fields = NULL;
  instance->fields = GROW_ARRAY(Value, instance->fields, 0, instance->fieldCap);
  popStack();

  return instance;
}

static void printFunction(ObjFunction *function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %.*s>", function->name->length, function->name->chars);
}

ObjNative *newNative(NativeFn function) {
  ObjNative *native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;

  return native;
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_NATIVE: {
    printf("<native fn>");
    break;
  }
  case OBJ_FUNCTION: {
    printFunction(AS_FUNCTION(value));
    break;
  }
  case OBJ_STRING: {
    ObjString *string = AS_STRING(value);
    printf("%.*s", string->length, string->chars);
    break;
  }
  case OBJ_STRUCT: {
    ObjString *string = AS_STRUCT(value)->name;
    printf("%.*s struct", string->length, string->chars);
    break;
  }
  case OBJ_INSTANCE: {
    ObjString *string = AS_INSTANCE(value)->name;
    printf("%.*s instance", string->length, string->chars);
    break;
  }
  case OBJ_ARRAY: {
    ObjArray *array = AS_ARRAY(value);
    printf("[");
    for (int i = 0; i < array->arrLen; i++) {
      printValue(array->arr[i]);
      if (i < array->arrLen - 1) {
        printf(",");
      }
    }
    printf("]");
    break;
  }
  case OBJ_MAP: {
    Table map = AS_MAP(value)->map;
    printf("{");
    for (int i = 0; i < map.count; i++) {
      Entry entry = map.entries[i];
      printf("'%s':", entry.key->chars);
      printValue(entry.value);
      if (i < map.count - 1) {
        printf(",");
      }
    }
    printf("}");
    break;
  }
  default: {
    printf("%d is unknown", OBJ_TYPE(value));
    break;
  }
  }
}

static ObjString *allocateString(char *chars, int length, uint32_t hash) {
  ObjString *string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;

  pushStack(OBJ_VAL(string));
  tableSet(&vm->strings, string, NIL_VAL);
  popStack();
  return string;
}
static uint32_t hashString(const char *key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }

  return hash;
}

ObjString *takeString(const char *chars, int length) {
  uint32_t hash = hashString(chars, length);

  ObjString *interned = tableFindString(&vm->strings, chars, length, hash);

  if (interned != NULL) {
    FREE_ARRAY(char, (char *)chars, length + 1);
    return interned;
  }
  return allocateString((char *)chars, length, hash);
}
ObjString *copyString(const char *chars, int length) {
  uint32_t hash = hashString(chars, length);
  ObjString *interned = tableFindString(&vm->strings, chars, length, hash);

  if (interned != NULL)
    return interned;

  char *heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';

  return allocateString(heapChars, length, hash);
}
