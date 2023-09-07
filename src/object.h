#ifndef cpplox_object_h
#define cpplox_object_h

#include "common.h"
#include "scanner.h"
#include "value.h"

#define CODE_MAX 500
#define CONST_MAX 100
#define MAP_MAXSIZE 500

typedef enum {
  OBJ_MAP,
  OBJ_INSTANCE,
  OBJ_STRUCT,
  OBJ_STRING,
  OBJ_FUNCTION,
  OBJ_NATIVE,
  OBJ_ARRAY
} ObjType;

struct Obj {
  ObjType type;
  bool isMarked;
  struct Obj *next;
};

typedef struct ObjFunction {
  Obj obj;
  int arity;
  ObjString *name;
  uint16_t *code;
  int *lines;
  int codeCap;
  int cp;
  int constP;
  Value *constants;
  int constCap;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value args);

typedef struct ObjNative {
  Obj obj;
  NativeFn function;
} ObjNative;

typedef struct ObjStruct {
  Obj obj;
  ObjString *name;
  ObjString**fields;
  int fieldLen;
  int fieldCap;
} ObjStruct;

typedef struct ObjArray {
  Obj obj;
  Value * arr;
  int arrLen;
  int arrCap;
} ObjArray;

typedef struct ObjInstance {
  Obj obj;
  ObjString *name;
  ObjStruct *strukt;
  int fieldLen;
  int fieldCap;
  Value *fields;
} ObjInstance;

typedef struct ObjMap {
  Obj obj;
  Value *keys;
  Value *values;
  int mapCap;
  int mapLen;
} ObjMap;

typedef struct ObjString {
  Obj obj;
  String string;
} ObjString;

#define IS_STRUCT(value) isObjType(value, OBJ_STRUCT)
#define IS_ARRAY(value) isObjType(value, OBJ_ARRAY)
#define IS_MAP(value) isObjType(value, OBJ_MAP)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value) isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define IS_STRING(value) isObjType(value, OBJ_STRING)

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define AS_ARRAY(value) ((ObjArray *)AS_OBJ(value))
#define AS_MAP(value) ((ObjMap *)AS_OBJ(value))
#define AS_STRUCT(value) ((ObjStruct *)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance *)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative *)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))

void freeChunk(ObjFunction *function);
void initChunk(ObjFunction *function);
void writeChunk(ObjFunction *function, uint16_t byte, int line);
void writeChunks(ObjFunction *function, uint16_t byte1, uint16_t byte2,
                 int line);
int addConstant(ObjFunction *function, Value value);
void printObject(Value value);
ObjFunction *newFunction();
ObjMap *newMap(int len);
ObjArray *newArray(int len);
ObjNative *newNative(NativeFn function);
ObjString *copyString(String string);
ObjStruct *newStruct(ObjString *name);
ObjInstance *newInstance(ObjStruct *strukt);
static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
