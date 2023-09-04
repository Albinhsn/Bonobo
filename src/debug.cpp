

#include "debug.h"
#include "common.h"
#include "value.h"

void disassembleChunk(ObjFunction *function, const char *name) {
  printf("== %s ==\n", name);

  for (int offset = 0; offset < function->cp;) {
    offset = disassembleInstruction(function, offset);
  }
}

static int simpleInstruction(const char *name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

static int byteInstruction(const char *name, ObjFunction *function,
                           int offset) {
  uint8_t slot = function->code[offset + 1];
  printf("%-16s %4d\n", name, slot);
  return offset + 2;
}

static int jumpInstruction(const char *name, int sign, ObjFunction *function,
                           int offset) {
  uint16_t jump = (uint16_t)(function->code[offset + 1] << 8);
  jump |= function->code[offset + 2];
  printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
  return offset + 3;
}

static int constantInstruction(const char *name, ObjFunction *function,
                               int offset) {
  uint8_t constant = function->code[offset + 1];
  printf("%s %d ", name, (int)constant);
  printValue(function->constants[constant]);
  printf("'\n");
  return offset + 2;
}

static int structArgInstruction(const char *name, ObjFunction *function,
                                int offset) {
  uint8_t constant = function->code[offset + 1];

  printf("%s %d ", name, (int)constant);
  // TODO something here?
  printf("'\n");
  return offset + 2;
}

std::string translateInstructions(std::vector<uint8_t> instructions) {
  int offset = 0;
  std::string translation = "";
  while (offset < instructions.size()) {
    switch (instructions[offset]) {
    case OP_CALL: {
      translation += "OP_CALL";
      // return byteInstruction("OP_CALL", chunk, offset);
    }
    case OP_INDEX: {
      translation += "OP_INDEX";
      // return byteInstruction("OP_INDEX", chunk, offset);
    }
    case OP_RETURN: {
      translation += "OP_RETURN";
      // return simpleInstruction("OP_RETURN", offset);
    }
    case OP_STRUCT_ARG: {
      translation += "OP_STRUCT_ARG";
      // return constantInstruction("OP_STRUCT_ARG", chunk, offset);
    }
    case OP_STRUCT: {
      translation += "OP_STRUCT";
      // return constantInstruction("OP_STRUCT", chunk, offset);
    }
    case OP_MAP: {
      translation += "OP_MAP";
      // return byteInstruction("OP_MAP", chunk, offset);
    }
    case OP_ARRAY: {
      translation += "OP_ARRAY";
      // return byteInstruction("OP_ARRAY", chunk, offset);
    }
    case OP_LOOP: {
      translation += "OP_LOOP";
      // return jumpInstruction("OP_LOOP", -1, chunk, offset);
    }
    case OP_PRINT: {
      translation += "OP_PRINT";
      // return simpleInstruction("OP_PRINT", offset);
    }
    case OP_JUMP: {
      translation += "OP_JUMP";
      // return jumpInstruction("OP_JUMP", 1, chunk, offset);
    }
    case OP_JUMP_IF_FALSE: {
      translation += "OP_JUMP_IF_FALSE";
      // return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
    }
    case OP_CONSTANT: {
      translation += "OP_CONSTANT";
      // return constantInstruction("OP_CONSTANT", chunk, offset);
    }
    case OP_NIL: {
      translation += "OP_NIL";
      // return simpleInstruction("OP_NIL", offset);
    }
    case OP_TRUE: {
      translation += "OP_TRUE";
      // return simpleInstruction("OP_TRUE", offset);
    }
    case OP_FALSE: {
      translation += "OP_FALSE";
      // return simpleInstruction("OP_FALSE", offset);
    }
    case OP_POP: {
      translation += "OP_POP";
      // return simpleInstruction("OP_POP", offset);
    }
    case OP_GET_LOCAL: {
      translation += "OP_GET_LOCAL";
      // return byteInstruction("OP_GET_LOCAL", chunk, offset);
    }
    case OP_SET_LOCAL: {
      translation += "OP_SET_LOCAL";
      // return byteInstruction("OP_SET_LOCAL", chunk, offset);
    }
    case OP_GET_GLOBAL: {
      translation += "OP_GET_GLOBAL";
      // return constantInstruction("OP_GET_GLOBAL", chunk, offset);
    }
    case OP_DEFINE_GLOBAL: {
      translation += "OP_DEFINE_GLOBAL";
      // return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
    }
    case OP_SET_GLOBAL: {
      translation += "OP_SET_GLOBAL";
      // return constantInstruction("OP_SET_GLOBAL", chunk, offset);
    }
    case OP_GET_PROPERTY: {
      translation += "OP_GET_PROPERTY";
      // return constantInstruction("OP_GET_PROPERTY", chunk, offset);
    }
    case OP_SET_PROPERTY: {
      translation += "OP_SET_PROPERTY";
      // return constantInstruction("OP_SET_PROPERTY", chunk, offset);
    }
    case OP_EQUAL: {
      translation += "OP_EQUAL";
      // return simpleInstruction("OP_EQUAL", offset);
    }
    case OP_GREATER: {
      translation += "OP_GREATER";
      // return simpleInstruction("OP_GREATER", offset);
    }
    case OP_LESS: {
      translation += "OP_LESS";
      // return simpleInstruction("OP_LESS", offset);
    }
    case OP_ADD: {
      translation += "OP_ADD";
      // return simpleInstruction("OP_ADD", offset);
    }
    case OP_SUBTRACT: {
      translation += "OP_SUBTRACT";
      // return simpleInstruction("OP_SUBTRACT", offset);
    }
    case OP_MULTIPLY: {
      translation += "OP_MULTIPLY";
      // return simpleInstruction("OP_MULTIPLY", offset);
    }
    case OP_DIVIDE: {
      translation += "OP_DIVIDE";
      // return simpleInstruction("OP_DIVIDE", offset);
    }
    case OP_NOT: {
      translation += "OP_NOT";
      // return simpleInstruction("OP_NOT", offset);
    }
    case OP_NEGATE: {
      translation += "OP_NEGATE";
      // return simpleInstruction("OP_NEGATE", offset);
    }
    default: {
      translation += "unknown opcode";
    }
    }
  }
  return translation;
}

int disassembleInstruction(ObjFunction *function, int offset) {
  printf("%d ", offset);
  if (offset > 0 && function->lines[offset] == function->lines[offset - 1]) {
    printf("  |   ");
  } else {
    printf(" %d   ", function->lines[offset]);
  }
  uint8_t instruction = function->code[offset];
  printf("%d\n", (int)instruction);
  switch (instruction) {
  case OP_CALL: {
    return byteInstruction("OP_CALL", function, offset);
  }
  case OP_INDEX: {
    return byteInstruction("OP_INDEX", function, offset);
  }
  case OP_RETURN: {
    return simpleInstruction("OP_RETURN", offset);
  }
  case OP_STRUCT_ARG: {
    return constantInstruction("OP_STRUCT_ARG", function, offset);
  }
  case OP_STRUCT: {
    return constantInstruction("OP_STRUCT", function, offset);
  }
  case OP_MAP: {
    return byteInstruction("OP_MAP", function, offset);
  }
  case OP_ARRAY: {
    return byteInstruction("OP_ARRAY", function, offset);
  }
  case OP_LOOP: {
    return jumpInstruction("OP_LOOP", -1, function, offset);
  }
  case OP_PRINT: {
    return simpleInstruction("OP_PRINT", offset);
  }
  case OP_JUMP: {
    return jumpInstruction("OP_JUMP", 1, function, offset);
  }
  case OP_JUMP_IF_FALSE: {
    return jumpInstruction("OP_JUMP_IF_FALSE", 1, function, offset);
  }
  case OP_CONSTANT: {
    return constantInstruction("OP_CONSTANT", function, offset);
  }
  case OP_NIL: {
    return simpleInstruction("OP_NIL", offset);
  }
  case OP_TRUE: {
    return simpleInstruction("OP_TRUE", offset);
  }
  case OP_FALSE: {
    return simpleInstruction("OP_FALSE", offset);
  }
  case OP_POP: {
    return simpleInstruction("OP_POP", offset);
  }
  case OP_GET_LOCAL: {
    return byteInstruction("OP_GET_LOCAL", function, offset);
  }
  case OP_SET_LOCAL: {
    return byteInstruction("OP_SET_LOCAL", function, offset);
  }
  case OP_GET_GLOBAL: {
    return constantInstruction("OP_GET_GLOBAL", function, offset);
  }
  case OP_DEFINE_GLOBAL: {
    return constantInstruction("OP_DEFINE_GLOBAL", function, offset);
  }
  case OP_SET_GLOBAL: {
    return constantInstruction("OP_SET_GLOBAL", function, offset);
  }
  case OP_GET_PROPERTY: {
    return constantInstruction("OP_GET_PROPERTY", function, offset);
  }
  case OP_SET_PROPERTY: {
    return constantInstruction("OP_SET_PROPERTY", function, offset);
  }
  case OP_EQUAL: {
    return simpleInstruction("OP_EQUAL", offset);
  }
  case OP_GREATER: {
    return simpleInstruction("OP_GREATER", offset);
  }
  case OP_LESS: {
    return simpleInstruction("OP_LESS", offset);
  }
  case OP_ADD: {
    return simpleInstruction("OP_ADD", offset);
  }
  case OP_SUBTRACT: {
    return simpleInstruction("OP_SUBTRACT", offset);
  }
  case OP_MULTIPLY: {
    return simpleInstruction("OP_MULTIPLY", offset);
  }
  case OP_DIVIDE: {
    return simpleInstruction("OP_DIVIDE", offset);
  }
  case OP_NOT: {
    return simpleInstruction("OP_NOT", offset);
  }
  case OP_NEGATE: {
    return simpleInstruction("OP_NEGATE", offset);
  }
  default: {
    printf("Unknown opcode %d\n", (int)instruction);
    return offset + 1;
  }
  }
}
