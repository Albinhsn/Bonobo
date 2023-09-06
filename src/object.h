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
};

typedef struct ObjFunction {
  Obj obj;
  int arity;
  ObjString *name;
  uint8_t *code;
  int *lines;
  int codeCap;
  int cp;
  int constP;
  Value *constants;
  int constCap;
  ObjFunction(Obj o, int a, ObjString *n)
      : obj(o), cp(0), constP(0), arity(a), name(n), codeCap(0), constCap(0),
        code(NULL), lines(NULL), constants(NULL){};
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value args);

typedef struct ObjNative {
  Obj obj;
  NativeFn function;
  ObjNative(Obj o, NativeFn fn) : obj(o), function(fn){};
} ObjNative;

typedef struct ObjStruct {
  Obj obj;
  ObjString *name;
  String *fields;
  int fieldLen;
  int fieldCap;
  ObjStruct(Obj o, ObjString *n)
      : obj(o), name(n), fieldLen(0), fieldCap(0), fields(NULL){};
} ObjStruct;

typedef struct ObjArray {
  Obj obj;
  Value arr[500];
  int arrLen;
  ObjArray(Obj o, int a) : obj(o), arrLen(a){};
} ObjArray;

typedef struct ObjInstance {
  Obj obj;
  ObjString *name;
  ObjStruct *strukt;
  int fieldLen;
  int fieldCap;
  Value *fields;
  ObjInstance(Obj o, ObjStruct *s)
      : obj(o), name(s->name), strukt(s), fieldLen(s->fieldLen), fields(NULL),
        fieldCap(0){};
} ObjInstance;

typedef struct ObjMap {
  Obj obj;
  Value *keys;
  Value *values;
  int mapCap;
  int mapLen;
  ObjMap(Obj o) : obj(o), mapLen(0), keys(NULL), values(NULL), mapCap(0){};
} ObjMap;

typedef struct ObjString {
  Obj obj;
  String string;
  ObjString(Obj o, String s) : obj(o), string(s){};
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
void writeChunk(ObjFunction *function, uint8_t byte, int line);
void writeChunks(ObjFunction *function, uint8_t byte1, uint8_t byte2, int line);
void addObject(Obj* obj);

int addConstant(ObjFunction *function, Value value);
Obj inline createObj(ObjType type) { return (Obj){type}; }
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
