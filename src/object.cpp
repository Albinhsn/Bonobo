
#include "object.h"
#include "value.h"
#include "vm.h"

void writeChunks(ObjFunction *function, uint8_t byte1, uint8_t byte2,
                 int line) {
  function->code[function->cp] = byte1;
  function->lines[function->cp] = line;
  function->cp++;

  function->code[function->cp] = byte2;
  function->lines[function->cp] = line;
  function->cp++;
}

void writeChunk(ObjFunction *function, uint8_t byte, int line) {
  function->code[function->cp] = byte;
  function->lines[function->cp++] = line;
}

int addConstant(ObjFunction *function, Value value) {
  function->constants[function->constP++] = value;
  return function->constP - 1;
}

ObjFunction *newFunction() {
  ObjFunction *function = new ObjFunction(createObj(OBJ_FUNCTION), 0, NULL);

  if (vm) {
    vm->objects[vm->op++] = (Obj *)function;
  }
  return function;
}
ObjMap *newMap(std::vector<Value> values) {
  ObjMap *mp = new ObjMap(createObj(OBJ_MAP), std::map<std::string, Value>());

  for (int i = 0; i < values.size(); i += 2) {
    mp->m[AS_STRING(values[i + 1])->chars] = values[i];
  }
  return mp;
}

ObjArray *newArray(std::vector<Value> values) {
  ObjArray *array = new ObjArray(createObj(OBJ_ARRAY), values);
  return array;
}

ObjStruct *newStruct(ObjString *name) {
  ObjStruct *strukt =
      new ObjStruct(createObj(OBJ_STRUCT), name, std::vector<std::string>());

  vm->objects[vm->op++] = (Obj *)strukt;
  return strukt;
}

ObjInstance *newInstance(ObjStruct *strukt, std::vector<Value> fields) {
  ObjInstance *instance =
      new ObjInstance(createObj(OBJ_INSTANCE), strukt->name, strukt, fields);

  vm->objects[vm->op++] = (Obj *)instance;
  return instance;
}

static void printFunction(ObjFunction *function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %s>", function->name->chars.c_str());
}

ObjNative *newNative(NativeFn function) {
  ObjNative *native = new ObjNative(createObj(OBJ_NATIVE), function);
  vm->objects[vm->op++] = (Obj *)function;

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
    std::cout << AS_STRING(value)->chars;
    break;
  }
  case OBJ_STRUCT: {
    std::cout << AS_STRUCT(value)->name->chars << " struct";
    break;
  }
  case OBJ_INSTANCE: {
    std::cout << AS_INSTANCE(value)->name->chars << " instance";
    break;
  }
  case OBJ_ARRAY: {
    ObjArray *array = AS_ARRAY(value);
    std::cout << "[";
    for (int i = 0; i < array->values.size(); i++) {
      printValue(array->values[i]);
      std::cout << (i < array->values.size() - 1 ? "," : "");
    }
    std::cout << "]";
    break;
  }
  case OBJ_MAP: {
    ObjMap *mp = AS_MAP(value);
    std::cout << "{";
    int i = 0;
    for (const auto &[key, value] : mp->m) {
      std::cout << "'" << key << "':";
      printValue(value);
      std::cout << (i < mp->m.size() - 1 ? "," : "");
      i++;
    }
    std::cout << "}";
    break;
  }
  default: {
    std::cout << OBJ_TYPE(value) << " is unknown";
  }
  }
}

ObjString *copyString(std::string str) {
  ObjString *objString = new ObjString(createObj(OBJ_STRING), str);
  vm->objects[vm->op++] = (Obj *)objString;

  return objString;
}
