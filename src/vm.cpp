
#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "scanner.h"
#include "value.h"
#include <cstdarg>
#include <cstdlib>
#include <cstring>

#define TWO63 0x8000000000000000u
#define TWO64f (TWO63 * 2.0)

VM vm;

static void defineNative(const char *name, int len, NativeFn function);

static Value clockNative(int argCount, Value args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void freeVM() { freeObjects(); }
void initVM() {
  vm.bytesAllocated = vm.fp = vm.grayCount = vm.grayCapacity = 0;
  vm.stackTop = vm.stack;
  vm.objects = NULL;
  initTable(&vm.globals);
  initTable(&vm.strings);
  vm.grayStack = NULL;
  vm.nextGC = 1024 * 1024;

  defineNative("clock", 5, clockNative);
}

void pushStack(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value popStack() {
  vm.stackTop--;
  return *vm.stackTop;
}

static void resetStack() { vm.stackTop = vm.stack; }

static void runtimeError(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);
  for (int i = 0; i < vm.fp; i++) {
    CallFrame *frame = &vm.frames[i];
    ObjFunction *function = frame->function;
    size_t instruction = frame->instructions[frame->ip];
    fprintf(stderr, "[line %d] in ", function->lines[instruction]);

    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%.*s()\n", function->name->length,
              function->name->chars);
    }
  }
  resetStack();
}

static inline int matchKey(const char *s, int l, ObjString *arr[], int arrLen) {
  for (int i = 0; i < arrLen; i++) {
    if (cmpString(s, l, arr[i]->chars, arr[i]->length)) {
      return i;
    }
  }
  return -1;
}

static void defineNative(const char *name, int len, NativeFn function) {
  pushStack(OBJ_VAL(copyString(name, len)));
  pushStack(OBJ_VAL(newNative(function)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  vm.stackTop -= 2;
}

static bool matchByte(OpCode code) {
  CallFrame *frame = &vm.frames[vm.fp - 1];
  if (frame->instructions[frame->ip] == code) {
    frame->ip++;
    return true;
  }
  return false;
}

static bool call(ObjFunction *function, int argCount) {
  if (argCount != function->arity) {
    runtimeError("Expected %d arguments but got %d", function->arity, argCount);
    return false;
  }
  if (vm.fp == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }
  CallFrame *frame = &vm.frames[vm.fp++];
  frame->function = function;
  frame->instructions = &function->code[0];
  frame->sp = vm.stackTop - argCount;
  frame->ip = 0;

  return true;
}

static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
    case OBJ_NATIVE: {
      NativeFn native = AS_NATIVE(callee);
      Value result = native(argCount, vm.stackTop[-1 - argCount]);
      // wtf is this xD
      vm.stackTop -= argCount + 1;
      *vm.stackTop = result;
      vm.stackTop++;
      return true;
    }
    case OBJ_FUNCTION: {
      return call(AS_FUNCTION(callee), argCount);
    }
    case OBJ_STRUCT: {
      ObjStruct *strukt = AS_STRUCT(callee);
      if (strukt->fieldLen != argCount) {
        runtimeError("Expected %d argument for struct but got %d",
                     strukt->fieldLen, argCount);
        return false;
      }

      ObjInstance *instance = newInstance(strukt);

      for (int i = argCount - 1; i >= 0; --i) {
        instance->fields[i] = popStack();
      }
      vm.stackTop[-1] = OBJ_VAL(instance);
      return true;
    }
    default:
      break;
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}
static bool setIndex() {
  Value item = vm.stackTop[-1];
  Value key = vm.stackTop[-2];
  Value seq = vm.stackTop[-3];
  vm.stackTop -= 3;
  switch (OBJ_TYPE(seq)) {
  case OBJ_ARRAY: {
    ObjArray *array = AS_ARRAY(seq);
    if (!IS_NUMBER(key)) {
      runtimeError("Can only index array with numbers");
      return false;
    }
    int k = (int)AS_NUMBER(key);
    if (k >= array->arrLen && k < 0) {
      runtimeError("Trying to access outside of array %d of size %d\n", k,
                   array->arrLen);
      return false;
    }
    array->arr[k] = item;
    pushStack(OBJ_VAL(array));
    return true;
  }
  case OBJ_MAP: {
    ObjMap *mp = AS_MAP(seq);
    if (!IS_STRING(key)) {
      runtimeError("Cant only access map with string");
      return false;
    }
    tableSet(&mp->map, AS_STRING(key), item);
    return true;
  }
  default: {
    runtimeError("Can only set index on array/map/string");
    return false;
  }
  }
}

static bool index() {

  Value key = vm.stackTop[-1];
  Value item = vm.stackTop[-2];
  vm.stackTop -= 2;

  if (!IS_OBJ(item)) {
    runtimeError("Can't only index array, map and string");
    return false;
  }

  switch (OBJ_TYPE(item)) {
  case OBJ_MAP: {
    ObjMap *mp = AS_MAP(item);
    Value value;
    if (!tableGet(&mp->map, AS_STRING(key), &value)) {
      runtimeError("Undefined variable '%s'.", AS_STRING(key)->chars);
      return INTERPRET_RUNTIME_ERROR;
    }
    pushStack(value);
  }
  case OBJ_STRING: {
    if (IS_NUMBER(key)) {
      runtimeError("Can only index string with number");
      return false;
    }
    ObjString *string = AS_STRING(item);
    int k = (int)AS_NUMBER(key);
    if (string->length <= k || k < 0) {
      runtimeError("Trying to access outside of array %d", k);
      return false;
    }
    pushStack(OBJ_VAL(copyString(&string->chars[k], 1)));
    return true;
  }
  case OBJ_ARRAY: {
    if (!IS_NUMBER(key)) {
      runtimeError("Can only index array with number");
      return false;
    }
    int k = (int)AS_NUMBER(key);
    ObjArray *array = AS_ARRAY(item);
    if (array->arrLen <= k || k < 0) {
      runtimeError("Trying to access outside of array %d", k);
      return false;
    }
    pushStack(array->arr[k]);
    return true;
  }
  default: {
    runtimeError("Can't only index array, map and string");
    return false;
  }
  }
}

static inline bool isFalsey(Value value) {
  return (IS_BOOL(value) && !AS_BOOL(value)) || IS_NIL(value);
}

InterpretResult run() {
  CallFrame *frame = &vm.frames[vm.fp - 1];
#define READ_SHORT()                                                           \
  (frame->ip += 2, (uint16_t)((frame->instructions[frame->ip - 2] << 8) |      \
                              frame->instructions[frame->ip - 1]))
#define READ_CONSTANT()                                                        \
  (frame->function->constants[frame->instructions[frame->ip++]])
#define MAP_DOUBLE(u) (((double)u) / TWO64f)
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(vm.stackTop[-1]) || !IS_NUMBER(vm.stackTop[-2])) {          \
      runtimeError("Operands must be numbers.");                               \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    vm.stackTop[-2] =                                                          \
        valueType(AS_NUMBER(vm.stackTop[-2]) op AS_NUMBER(vm.stackTop[-1]));   \
    vm.stackTop--;                                                             \
  } while (false)

  for (;;) {
    uint16_t *instructions = frame->instructions;
    // printf("stackSize %d\n", (int)(vm->stackTop - vm->stack));
#ifdef DEBUG_TRACE_EXECUTION
    printf("        ");
    for (Value *slot = vm->stack; slot < vm->stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(frame->function, (int)frame->ip);
#endif
    switch (instructions[frame->ip++]) {
    case OP_CONSTANT: {
      pushStack(READ_CONSTANT());
      break;
    }
    case OP_NIL: {
      pushStack(NIL_VAL);
      break;
    }
    case OP_TRUE: {
      pushStack(BOOL_VAL(true));
      break;
    }
    case OP_FALSE: {
      pushStack(BOOL_VAL(false));
      break;
    }
    case OP_POP: {
      vm.stackTop--;
      break;
    }
    case OP_GET_LOCAL: {
      pushStack(frame->sp[instructions[frame->ip++]]);
      break;
    }
    case OP_SET_LOCAL: {
      uint16_t slot = instructions[frame->ip++];
      frame->sp[slot] = vm.stackTop[-1];
      break;
    }
    case OP_GET_GLOBAL: {
      ObjString *name = READ_STRING();
      Value value;

      if (!tableGet(&vm.globals, name, &value)) {
        runtimeError("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }

      pushStack(value);
      break;
    }
    case OP_DEFINE_GLOBAL: {
      ObjString *name = READ_STRING();
      tableSet(&vm.globals, name, vm.stackTop[-1]);
      vm.stackTop--;
      break;
    }
    case OP_SET_GLOBAL: {
      ObjString *name = READ_STRING();
      if (tableSet(&vm.globals, name, vm.stackTop[-1])) {
        tableDelete(&vm.globals, name);
        runtimeError("Undefined variable '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_GET_PROPERTY: {
      if (!IS_INSTANCE(vm.stackTop[-1])) {
        runtimeError("Only instances have properties.");
        return INTERPRET_RUNTIME_ERROR;
      }

      ObjInstance *instance = AS_INSTANCE(vm.stackTop[-1]);
      ObjString *fieldName = AS_STRING(READ_CONSTANT());

      int idx = matchKey(fieldName->chars, fieldName->length,
                         instance->strukt->fields, instance->strukt->fieldLen);
      if (idx == -1) {
        runtimeError("Couldn't find field?");
        return INTERPRET_RUNTIME_ERROR;
      }

      vm.stackTop[-1] = instance->fields[idx];
      break;
    }
    case OP_SET_PROPERTY: {
      Value v1 = vm.stackTop[-2];
      if (!IS_INSTANCE(v1)) {
        runtimeError("Only instances have fields.");
        return INTERPRET_RUNTIME_ERROR;
      }

      ObjInstance *instance = AS_INSTANCE(v1);
      instance->fields[(int)instructions[frame->ip++]] = vm.stackTop[-1];
      // might need to cast here?
      vm.stackTop[-1] = v1;
      break;
    }
    case OP_EQUAL: {
      vm.stackTop[-2] = BOOL_VAL(valuesEqual(vm.stackTop[-1], vm.stackTop[-2]));
      vm.stackTop--;
      break;
    }
    case OP_GREATER_EQUAL: {
      BINARY_OP(BOOL_VAL, >=);
      break;
    }
    case OP_GREATER: {
      BINARY_OP(BOOL_VAL, >);
      break;
    }
    case OP_LESS: {
      BINARY_OP(BOOL_VAL, <);
      break;
    }
    case OP_LESS_EQUAL: {
      BINARY_OP(BOOL_VAL, <=);
      break;
    }
    case OP_ADD: {
      if (IS_NUMBER(vm.stackTop[-1]) && IS_NUMBER(vm.stackTop[-2])) {
        vm.stackTop[-2] =
            NUMBER_VAL(AS_NUMBER(vm.stackTop[-2]) + AS_NUMBER(vm.stackTop[-1]));
        vm.stackTop--;
      } else if (IS_STRING(vm.stackTop[-1]) && IS_STRING(vm.stackTop[-2])) {
        ObjString *s1 = AS_STRING(popStack());
        ObjString *s2 = AS_STRING(vm.stackTop[-2]);

        char s[s1->length + s2->length];
        strcpy(s, s1->chars);
        strcat(s, s2->chars);
        vm.stackTop[-1] = OBJ_VAL(copyString(s, s1->length + s2->length));

      } else {
        runtimeError("Operands must be two number or two strings");
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_SUBTRACT: {
      BINARY_OP(NUMBER_VAL, -);
      break;
    }
    case OP_MULTIPLY: {
      BINARY_OP(NUMBER_VAL, *);
      break;
    }
    case OP_DIVIDE: {
      BINARY_OP(NUMBER_VAL, /);
      break;
    }
    case OP_NOT: {
      vm.stackTop[-1] = BOOL_VAL(isFalsey(vm.stackTop[-1]));
      break;
    }
    case OP_NEGATE: {
      if (!IS_NUMBER(vm.stackTop[-1])) {
        runtimeError("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }
      vm.stackTop[-1] = NUMBER_VAL(-AS_NUMBER(vm.stackTop[-1]));
      break;
    }
    case OP_PRINT: {
      printValue(popStack());
      printf("\n");
      break;
    }
    case OP_JUMP: {
      uint16_t offset = READ_SHORT();
      frame->ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      uint16_t offset = READ_SHORT();
      if (isFalsey(vm.stackTop[-1])) {
        frame->ip += offset;
      }
      break;
    }
    case OP_LOOP: {
      uint16_t offset = READ_SHORT();
      frame->ip -= offset;
      break;
    }
    case OP_INDEX: {
      if (!index()) {
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_SET_INDEX: {
      if (!IS_OBJ(vm.stackTop[-3])) {
        runtimeError("Can only assign index to map/array/string");
        return INTERPRET_RUNTIME_ERROR;
      }
      if (!setIndex()) {
        return INTERPRET_RUNTIME_ERROR;
      }
      break;
    }
    case OP_CALL: {
      int argCount = instructions[frame->ip++];
      if (!callValue(vm.stackTop[-1 - argCount], argCount)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = &vm.frames[vm.fp - 1];
      break;
    }
    case OP_ARRAY: {
      int argCount = instructions[frame->ip++];
      ObjArray *array = newArray(argCount);
      for (int i = argCount - 1; i >= 0; i--) {
        if (array->arrCap < array->arrLen + 1) {
          int oldCapacity = array->arrCap;
          array->arrCap = GROW_CAPACITY(oldCapacity);
          array->arr =
              GROW_ARRAY(Value, array->arr, oldCapacity, array->arrCap);
        }
        array->arr[i] = popStack();
      }
      pushStack(OBJ_VAL(array));
      break;
    }
    case OP_MAP: {
      int argCount = instructions[frame->ip++];
      ObjMap *mp = newMap();
      for (int i = argCount - 1; i >= 0; i--) {
        tableSet(&mp->map, AS_STRING(vm.stackTop[-1]), vm.stackTop[-2]);
        vm.stackTop -= 2;
      }
      pushStack(OBJ_VAL(mp));
      break;
    }
    case OP_STRUCT: {
      ObjString *name = AS_STRING(READ_CONSTANT());

      // This should be handled in the compiler?
      // if (vm->globals.count(name->chars)) {
      //   runtimeError("Can't redeclare a struct '" + name->chars + "'.");
      //   return INTERPRET_RUNTIME_ERROR;
      // }

      ObjStruct *strukt = newStruct(name);

      pushStack(OBJ_VAL(strukt));
      while (matchByte(OP_STRUCT_ARG)) {
        if (strukt->fieldCap < strukt->fieldLen + 1) {
          int oldCapacity = strukt->fieldCap;
          strukt->fieldCap = GROW_CAPACITY(oldCapacity);
          strukt->fields = GROW_ARRAY(ObjString *, strukt->fields, oldCapacity,
                                      strukt->fieldCap);
        }
        strukt->fields[strukt->fieldLen++] = AS_STRING(READ_CONSTANT());
      }
      vm.stackTop[-1] = OBJ_VAL(strukt);
      break;
    }
    case OP_RETURN: {
      Value result = popStack();

      // free frame
      vm.fp--;
      vm.stackTop = frame->sp;

      if (vm.fp == 0) {
        vm.stackTop--;
        return INTERPRET_OK;
      }
      vm.stackTop[-1] = result;
      frame = &vm.frames[vm.fp - 1];
      break;
    }
    }
  }

#undef READ_CONSTANT
#undef READ_STRING
#undef READ_SHORT
#undef BINARY_OP
}

InterpretResult interpret(const char *source) {
  initVM();
  Compiler *compiler = compile(source);
  if (compiler == NULL) {
    return INTERPRET_COMPILE_ERROR;
  }
  ObjFunction *function = compiler->function;
  pushStack(OBJ_VAL(function));
  call(function, 0);
  freeCompiler(compiler);
  InterpretResult result = run();
  freeVM();
  return result;
}
