#ifndef cpplox_object_h
#define cpplox_object_h

#include "common.h"
#include "scanner.h"
#include "value.h"
#include <map>

#define CODE_MAX 500
#define CONST_MAX 100

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
  uint8_t code[CODE_MAX];
  int lines[CODE_MAX];
  int cp;
  int constP;
  Value constants[CONST_MAX];
  ObjFunction(Obj o, int a, ObjString *n)
      : obj(o), cp(0), constP(0), arity(a), name(n){};
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
  std::vector<String> fields;
  ObjStruct(Obj o, ObjString *n, std::vector<String> f)
      : obj(o), name(n), fields(f){};
} ObjStruct;

typedef struct ObjArray {
  Obj obj;
  std::vector<Value> values;
  ObjArray(Obj o, std::vector<Value> v) : obj(o), values(v){};
} ObjArray;

typedef struct ObjInstance {
  Obj obj;
  ObjString *name;
  ObjStruct *strukt;
  std::vector<Value> fields;
  ObjInstance(Obj o, ObjString *n, ObjStruct *s, std::vector<Value> f)
      : obj(o), name(n), strukt(s), fields(f){};
} ObjInstance;

typedef struct ObjMap {
  Obj obj;
  std::map<String, Value> m;
  ObjMap(Obj o, std::map<String, Value> M) : obj(o), m(M){};
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

int addConstant(ObjFunction *function, Value value);
Obj inline createObj(ObjType type) { return (Obj){type}; }
void printObject(Value value);
ObjFunction *newFunction();
ObjMap *newMap(std::vector<Value>);
ObjArray *newArray(std::vector<Value>);
ObjNative *newNative(NativeFn function);
ObjString *copyString(String string);
ObjStruct *newStruct(ObjString *name);
ObjInstance *newInstance(ObjStruct *strukt, std::vector<Value> fields);
static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
