
#include "vm.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "value.h"
#include <algorithm>
#include <cstdarg>
#include <cstring>
#include <map>
#include <time.h>

VM *vm;

static void defineNative(const char *name, int len, NativeFn function);

static Value clockNative(int argCount, Value args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void initVM() {
  vm = new VM();
  vm->stackTop = vm->stack;
  vm->op = vm->fp = vm->gp = 0;

  defineNative("clock", 5, clockNative);
}

void inline pushStack(Value value) {
  *vm->stackTop = value;
  vm->stackTop++;
}

Value inline popStack() {
  vm->stackTop--;
  return *vm->stackTop;
}

static void resetStack() { vm->stackTop = vm->stack; }

static void runtimeError(const char *format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);
  for (int i = 0; i < vm->fp; i++) {
    CallFrame *frame = vm->frames[i];
    ObjFunction *function = frame->function;
    size_t instruction = frame->instructions[frame->ip];
    fprintf(stderr, "[line %d] in ", function->lines[instruction]);

    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%.*s()\n", function->name->string.length,
              function->name->string.literal);
    }
  }
  resetStack();
}

static void defineNative(const char *name, int len, NativeFn function) {
  String s = newString(name, len);
  ObjString *string = copyString(s);
  ObjNative *native = newNative(function);

  vm->objects[vm->op++] = (Obj *)string;
  vm->objects[vm->op++] = (Obj *)native;
  vm->globalKeys[vm->gp] = s;
  vm->globalValues[vm->gp++] = OBJ_VAL((Obj *)native);
}

static bool matchByte(OpCode code) {
  CallFrame *frame = vm->frames[vm->fp - 1];
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
  if (vm->fp == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }
  CallFrame *frame = new CallFrame;
  frame->function = function;
  frame->instructions = &function->code[0];
  frame->sp = vm->stackTop - argCount;
  frame->ip = 0;
  vm->frames[vm->fp++] = frame;

  return true;
}

static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
    case OBJ_NATIVE: {
      NativeFn native = AS_NATIVE(callee);
      Value result = native(argCount, vm->stackTop[-1 - argCount]);
      // wtf is this xD
      vm->stackTop -= argCount + 1;
      *vm->stackTop = result;
      vm->stackTop++;
      return true;
    }
    case OBJ_FUNCTION: {
      return call(AS_FUNCTION(callee), argCount);
    }
    case OBJ_STRUCT: {
      ObjStruct *strukt = AS_STRUCT(callee);
      if (strukt->fields.size() != argCount) {
        runtimeError("Expected %d argument for struct but got %d",
                     strukt->fields.size(), argCount);
        return false;
      }
      std::vector<Value> fields;
      // Do another function for this
      for (int i = 0; i < argCount; ++i) {
        fields.push_back(popStack());
      }
      vm->stackTop[-1] = OBJ_VAL(newInstance(strukt, fields));
      return true;
    }
    default:
      break;
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}

static bool index() {
  Value key = vm->stackTop[-1];
  Value item = vm->stackTop[-2];
  vm->stackTop -= 2;
  if (item.type != VAL_OBJ) {
    runtimeError("Can't only index array, map and string");
    return false;
  }
  switch (OBJ_TYPE(item)) {
  case OBJ_MAP: {
    ObjMap *mp = AS_MAP(item);
    ObjString *string = AS_STRING(key);

    // if (mp->m.count(string->string)) {
    //   pushStack(mp->m[string->string]);
    //   return true;
    // }
    runtimeError("Trying to access map with unknown key %.*s",
                 string->string.length, string->string.literal);
    return false;
  }
  case OBJ_STRING: {
    if (key.type != VAL_NUMBER) {
      runtimeError("Can only index string with number");
      return false;
    }
    ObjString *string = AS_STRING(item);
    int k = (int)key.as.number;
    // if (string->chars <= k || k < 0) {
    //   runtimeError("Trying to access outside of array %d", k);
    //   return false;
    // }

    // pushStack(OBJ_VAL(copyString(string->chars.substr(k, 1))));
    return true;
  }
  case OBJ_ARRAY: {
    if (key.type != VAL_NUMBER) {
      runtimeError("Can only index array with number");
      return false;
    }
    int k = (int)key.as.number;
    ObjArray *array = AS_ARRAY(item);
    if (array->values.size() <= k || k < 0) {
      runtimeError("Trying to access outside of array %d", k);
      return false;
    }
    pushStack(array->values[k]);
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
  CallFrame *frame = vm->frames[vm->fp - 1];
#define READ_SHORT()                                                           \
  (frame->ip += 2, (uint16_t)((frame->instructions[frame->ip - 2] << 8) |      \
                              frame->instructions[frame->ip - 1]))
#define READ_CONSTANT()                                                        \
  (frame->function->constants[frame->instructions[frame->ip++]])
#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op)                                               \
  do {                                                                         \
    if (!IS_NUMBER(vm->stackTop[-1]) || !IS_NUMBER(vm->stackTop[-2])) {        \
      runtimeError("Operands must be numbers.");                               \
      return INTERPRET_RUNTIME_ERROR;                                          \
    }                                                                          \
    vm->stackTop[-2] =                                                         \
        valueType(AS_NUMBER(vm->stackTop[-2]) op AS_NUMBER(vm->stackTop[-1])); \
    vm->stackTop--;                                                            \
  } while (false)

  for (;;) {
    uint8_t *instructions = frame->instructions;
#ifdef DEBUG_TRACE_EXECUTION
    std::cout << "        ";
    for (Value *slot = vm->stack; slot < vm->stackTop; slot++) {
      std::cout << "[ ";
      printValue(*slot);
      std::cout << " ]";
    }
    std::cout << "\n";
    disassembleInstruction(frame->function->chunk, (int)frame->ip);
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
      vm->stackTop--;
      break;
    }
    case OP_GET_LOCAL: {
      pushStack(frame->sp[instructions[frame->ip++]]);
      break;
    }
    case OP_SET_LOCAL: {
      uint8_t slot = instructions[frame->ip++];
      frame->sp[slot] = vm->stackTop[-1];
      break;
    }
    case OP_GET_GLOBAL: {
      ObjString *string = READ_STRING();
      String s = string->string;
      int i = 0;
      while (i < vm->gp) {
        if (s.length == vm->globalKeys[i].length &&
            memcmp(s.literal, vm->globalKeys[i].literal, s.length) == 0) {
          break;
        }
        i++;
      }
      if (i == vm->gp) {
        runtimeError("Undefined variable '%.*s'.", s.length, s.literal);
        return INTERPRET_RUNTIME_ERROR;
      }
      *vm->stackTop = vm->globalValues[i];
      vm->stackTop++;
      break;
    }
    case OP_DEFINE_GLOBAL: {
      String s = READ_STRING()->string;
      vm->globalKeys[vm->gp] = s;
      vm->globalValues[vm->gp] = popStack();
      vm->gp++;
      break;
    }
    case OP_SET_GLOBAL: {
      ObjString *string = READ_STRING();
      String s = string->string;
      int i = 0;
      while (i < vm->gp) {
        if (cmpString(string->string, vm->globalKeys[i])) {
          break;
        }
        i++;
      }
      if (i == vm->gp) {
        runtimeError("Undefined variable '%.*s'.", s.length, s.literal);
        return INTERPRET_RUNTIME_ERROR;
      }
      vm->globalValues[i] = vm->stackTop[-1];
      break;
    }
    case OP_GET_PROPERTY: {
      if (!IS_INSTANCE(vm->stackTop[-1])) {
        runtimeError("Only instances have properties.");
        return INTERPRET_RUNTIME_ERROR;
      }
      // Get the instance from the top
      ObjInstance *instance = AS_INSTANCE(vm->stackTop[-1]);
      String fieldName = AS_STRING(READ_CONSTANT())->string;
      std::vector<String> struktFields = instance->strukt->fields;
      int idx = -1;
      for (int i = 0; i < struktFields.size(); ++i) {
        if (cmpString(struktFields[i], fieldName)) {
          idx = i;
          break;
        }
      }
      if (idx == -1) {
        runtimeError("Couldn't find field?");
        return INTERPRET_RUNTIME_ERROR;
      }

      // Update aka remove the instance and replace it with the field
      vm->stackTop[-1] = instance->fields[idx];
      break;
    }
    case OP_SET_PROPERTY: {
      Value v1 = vm->stackTop[-2];
      if (!IS_INSTANCE(v1)) {
        std::cout << OBJ_TYPE(v1) << "\n";
        runtimeError("Only instances have fields.");
        return INTERPRET_RUNTIME_ERROR;
      }

      ObjInstance *instance = AS_INSTANCE(v1);
      instance->fields[(int)instructions[frame->ip++]] = vm->stackTop[-1];
      // might need to cast here?
      vm->stackTop[-1] = v1;
      break;
    }
    case OP_EQUAL: {
      vm->stackTop[-2] =
          BOOL_VAL(valuesEqual(vm->stackTop[-1], vm->stackTop[-2]));
      vm->stackTop--;
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
      if (IS_NUMBER(vm->stackTop[-1]) && IS_NUMBER(vm->stackTop[-2])) {
        vm->stackTop[-2] = NUMBER_VAL(AS_NUMBER(vm->stackTop[-2]) +
                                      AS_NUMBER(vm->stackTop[-1]));
        vm->stackTop--;
      } else if (IS_STRING(vm->stackTop[-1]) && IS_STRING(vm->stackTop[-2])) {
        String s1 = AS_STRING(popStack())->string;
        String s2 = AS_STRING(vm->stackTop[-2])->string;

        char s[s1.length + s2.length];
        strcpy(s, s1.literal);
        strcat(s, s2.literal);
        vm->stackTop[-1] =
            OBJ_VAL(copyString(newString(s, s1.length + s2.length)));

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
      vm->stackTop[-1] = BOOL_VAL(isFalsey(vm->stackTop[-1]));
      break;
    }
    case OP_NEGATE: {
      if (!IS_NUMBER(vm->stackTop[-1])) {
        runtimeError("Operand must be a number.");
        return INTERPRET_RUNTIME_ERROR;
      }
      vm->stackTop[-1] = NUMBER_VAL(-AS_NUMBER(vm->stackTop[-1]));
      break;
    }
    case OP_PRINT: {
      printValue(popStack());
      std::cout << "\n";
      break;
    }
    case OP_JUMP: {
      uint16_t offset = READ_SHORT();
      frame->ip += offset;
      break;
    }
    case OP_JUMP_IF_FALSE: {
      uint16_t offset = READ_SHORT();
      if (isFalsey(vm->stackTop[-1])) {
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
    case OP_CALL: {
      int argCount = instructions[frame->ip++];
      if (!callValue(vm->stackTop[-1 - argCount], argCount)) {
        return INTERPRET_RUNTIME_ERROR;
      }
      frame = vm->frames[vm->fp - 1];
      break;
    }
    case OP_ARRAY: {
      int argCount = instructions[frame->ip++];
      std::vector<Value> values = std::vector<Value>(argCount);
      for (int i = 0; i < argCount; i++) {
        values[i] = popStack();
      }
      pushStack(OBJ_VAL(newArray(values)));
      break;
    }
    case OP_MAP: {
      int argCount = instructions[frame->ip++];
      // Rework this into stack function?
      std::vector<Value> values = std::vector<Value>(argCount);
      for (int i = 0; i < argCount; i++) {
        values[i] = popStack();
      }
      pushStack(OBJ_VAL(newMap(values)));
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
      while (matchByte(OP_STRUCT_ARG)) {
        strukt->fields.push_back(AS_STRING(READ_CONSTANT())->string);
      }
      std::reverse(strukt->fields.begin(), strukt->fields.end());
      vm->globalKeys[vm->gp] = name->string;
      vm->globalValues[vm->gp] = OBJ_VAL(strukt);
      vm->gp++;
      break;
    }
    case OP_RETURN: {
      Value result = popStack();
      vm->stackTop = freeFrame(vm);
      if (vm->fp == 0) {
        popStack();
        return INTERPRET_OK;
      }
      vm->stackTop[-1] = result;
      frame = vm->frames[vm->fp - 1];
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
  return result;
}
