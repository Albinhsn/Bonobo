
#include "object.h"
#include "memory.h"
#include "value.h"
#include "vm.h"

void writeChunks(ObjFunction *function, uint16_t byte1, uint16_t byte2,
                 int line) {
  writeChunk(function, byte1, line);
  writeChunk(function, byte2, line);
}
void addObject(Obj *obj) {

  if (vm->objCap < vm->objLen + 1) {
    int oldCapacity = vm->objCap;
    vm->objCap = GROW_CAPACITY(oldCapacity);
    vm->objects = GROW_ARRAY(Obj *, vm->objects, oldCapacity, vm->objCap);
  }
  vm->objects[vm->objLen++] = obj;
}

void writeChunk(ObjFunction *function, uint16_t byte, int line) {
  if (function->codeCap < function->cp + 1) {
    int oldCapacity = function->codeCap;
    function->codeCap = GROW_CAPACITY(oldCapacity);
    function->code =
        GROW_ARRAY(uint16_t, function->code, oldCapacity, function->codeCap);
    function->lines =
        GROW_ARRAY(int, function->lines, oldCapacity, function->codeCap);
  }
  function->code[function->cp] = byte;
  function->lines[function->cp++] = line;
}

int addConstant(ObjFunction *function, Value value) {
  if (function->constCap < function->constP + 1) {
    int oldCapacity = function->constCap;
    function->constCap = GROW_CAPACITY(oldCapacity);
    function->constants =
        GROW_ARRAY(Value, function->constants, oldCapacity, function->constCap);
  }

  function->constants[function->constP++] = value;
  return function->constP - 1;
}

ObjFunction *newFunction() {
  ObjFunction *function = NULL;
  function = (ObjFunction *)malloc(sizeof(ObjFunction));
  function->arity = function->constP = function->constCap = function->cp = 0;
  function->obj = createObj(OBJ_FUNCTION);
  function->code = NULL;
  function->lines = NULL;
  function->constants = NULL;

  if (vm) {
    addObject((Obj *)function);
  }
  return function;
}

ObjMap *newMap(int len) {
  ObjMap *mp = NULL;
  mp = (ObjMap *)malloc(sizeof(ObjMap));
  mp->obj = createObj(OBJ_MAP);
  mp->mapLen = len;
  mp->keys = NULL;
  mp->values = NULL;
  mp->mapCap = 0;
  mp->mapLen = len;

  addObject((Obj *)mp);
  return mp;
}

ObjArray *newArray(int len) {
  ObjArray *array = NULL;
  array = (ObjArray *)malloc(sizeof(ObjArray *));
  array->obj = createObj(OBJ_ARRAY);
  array->arrLen = len;

  return array;
}

ObjStruct *newStruct(ObjString *name) {
  ObjStruct *strukt = NULL;
  strukt = (ObjStruct *)malloc(sizeof(ObjStruct));
  strukt->obj = createObj(OBJ_STRUCT);
  strukt->name = name;
  strukt->fieldCap = 0;
  strukt->fields = NULL;
  strukt->fieldLen = 0;

  addObject((Obj *)strukt);
  return strukt;
}

ObjInstance *newInstance(ObjStruct *strukt) {
  ObjInstance *instance = NULL;
  instance->obj = createObj(OBJ_STRUCT);
  instance->fieldLen = 0;
  instance->fieldCap = strukt->fieldCap;
  instance->fields = GROW_ARRAY(Value, instance->fields, 0, instance->fieldCap);
  instance->name = strukt->name;
  instance->strukt = strukt;

  addObject((Obj *)instance);
  return instance;
}

static void printFunction(ObjFunction *function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %.*s>", function->name->string.length,
         function->name->string.literal);
}

ObjNative *newNative(NativeFn function) {
  ObjNative *native = NULL;
  native = (ObjNative *)malloc(sizeof(ObjNative));
  native->obj = createObj(OBJ_NATIVE);
  native->function = function;

  addObject((Obj *)native);

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
    printf("%.*s", string->string.length, string->string.literal);
    break;
  }
  case OBJ_STRUCT: {
    ObjString *string = AS_STRUCT(value)->name;
    printf("%.*s struct", string->string.length, string->string.literal);
    break;
  }
  case OBJ_INSTANCE: {
    ObjString *string = AS_INSTANCE(value)->name;
    printf("%.*s instance", string->string.length, string->string.literal);
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
    ObjMap *mp = AS_MAP(value);
    printf("{");
    for (int i = 0; i < mp->mapLen; i++) {
      if (IS_STRING(mp->keys[i])) {
        ObjString *string = AS_STRING(mp->keys[i]);
        printf("'%.*s':", string->string.length, string->string.literal);
        printValue(mp->values[i]);
        if (i < mp->mapLen - 1) {
          printf(",");
        }
      }
      if (IS_NUMBER(mp->keys[i])) {
        printf("%.0lf:", AS_NUMBER(mp->keys[i]));
        printValue(mp->values[i]);
        if (i < mp->mapLen - 1) {
          printf(",");
        }
      }
    }
    printf("}");
    break;
  }
  default: {
    printf("%d is unknown", OBJ_TYPE(value));
  }
  }
}

ObjString *copyString(String string) {
  ObjString *objString = NULL;
  objString = (ObjString *)malloc(sizeof(ObjString));
  objString->string = string;
  objString->obj = createObj(OBJ_STRING);

  addObject((Obj *)objString);

  return objString;
}
