#ifndef cpplox_value_h
#define cpplox_value_h

#include "common.h"

typedef struct ObjNative ObjNative;
typedef struct ObjFunction ObjFunction;
typedef struct ObjString ObjString;
typedef struct ObjStruct ObjStruct;
typedef struct ObjArray ObjArray;
typedef struct ObjInstance ObjInstance;
typedef struct ObjMap ObjMap;

typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_INT,
  VAL_FLOAT,
  VAL_ARRAY,
  VAL_MAP,
  VAL_STRUCT
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double dbl;
    int integer;
    ObjArray *array;
    ObjFunction *function;
    ObjInstance *instance;
    ObjNative *native;
    ObjStruct *strukt;
  } as;
} Value;

#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_NIL(value) ((value).type == VAL_NIL)

#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)

#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#endif

void printValue(Value value);
bool valuesEqual(Value a, Value b);
