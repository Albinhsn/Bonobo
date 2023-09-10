#ifndef cpplox_object_h
#define cpplox_object_h

#include "common.h"
#include "scanner.h"
#include "value.h"
#include <map>

typedef struct ObjString {
  int length;
  uint32_t hash;
  char *chars;
} ObjString;

typedef struct ObjFunction {
  int arity;
  ObjString *name;
  uint16_t *code;
  int *lines;
  int codeCap;
  int codeP;
  int constP;
  Value *constants;
  int constCap;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value *args);

typedef struct ObjNative {
  NativeFn function;
} ObjNative;

typedef struct ObjStruct {
  ObjString *name;
  ObjString **fields;
  int fieldLen;
  int fieldCap;
} ObjStruct;

typedef struct ObjArray {
  Value *arr;
  int arrLen;
  int arrCap;
} ObjArray;

typedef struct ObjInstance {
  ObjString *name;
  ObjStruct *strukt;
  int fieldLen;
  int fieldCap;
  Value *fields;
} ObjInstance;

typedef struct ObjMap {
  std::map<Value, Value> map;
} ObjMap;

void freeChunk(ObjFunction *function);
void initChunk(ObjFunction *function);
void writeChunk(ObjFunction *function, uint16_t byte, int line);
void writeChunks(ObjFunction *function, uint16_t byte1, uint16_t byte2,
                 int line);
int addConstant(ObjFunction *function, Value value);
void printObject(Value value);
ObjFunction *newFunction();
ObjMap *newMap();
ObjArray *newArray(int len);
ObjNative *newNative(NativeFn function);
ObjString *copyString(const char *chars, int length);
ObjStruct *newStruct(ObjString *name);
ObjInstance *newInstance(ObjStruct *strukt);

#endif
