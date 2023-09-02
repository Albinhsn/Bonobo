

#include "debug.h"
#include "common.h"
#include "value.h"

void disassembleChunk(Chunk *chunk, std::string name) {
  std::cout << "== " << name << " ==\n";

  for (int offset = 0; offset < chunk->cp;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

static int simpleInstruction(std::string name, int offset) {
  std::cout << name << "\n";
  return offset + 1;
}

static int byteInstruction(const char *name, Chunk *chunk, int offset) {
  uint8_t slot = chunk->code[offset + 1];
  printf("%-16s %4d\n", name, slot);
  return offset + 2;
}

static int jumpInstruction(const char *name, int sign, Chunk *chunk,
                           int offset) {
  uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
  jump |= chunk->code[offset + 2];
  printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
  return offset + 3;
}

static int constantInstruction(std::string name, Chunk *chunk, int offset) {
  uint8_t constant = chunk->code[offset + 1];
  std::cout << name << " " << (int)constant << " '";
  printValue(chunk->constants[constant]);
  std::cout << "'\n";
  return offset + 2;
}

static int structArgInstruction(std::string name, Chunk *chunk, int offset) {
  uint8_t constant = chunk->code[offset + 1];
  std::cout << name << " " << (int)constant << " '";
  std::cout << "'\n";
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

int disassembleInstruction(Chunk *chunk, int offset) {
  std::cout << offset << " ";
  if (offset > 0 && chunk->lines[offset] == chunk->lines[offset - 1]) {
    std::cout << "  |   ";
  } else {
    std::cout << chunk->lines[offset] << "   ";
  }
  uint8_t instruction = chunk->code[offset];
  std::cout << (int)instruction << "\n";
  switch (instruction) {
  case OP_CALL: {
    return byteInstruction("OP_CALL", chunk, offset);
  }
  case OP_INDEX: {
    return byteInstruction("OP_INDEX", chunk, offset);
  }
  case OP_RETURN: {
    return simpleInstruction("OP_RETURN", offset);
  }
  case OP_STRUCT_ARG: {
    return constantInstruction("OP_STRUCT_ARG", chunk, offset);
  }
  case OP_STRUCT: {
    return constantInstruction("OP_STRUCT", chunk, offset);
  }
  case OP_MAP: {
    return byteInstruction("OP_MAP", chunk, offset);
  }
  case OP_ARRAY: {
    return byteInstruction("OP_ARRAY", chunk, offset);
  }
  case OP_LOOP: {
    return jumpInstruction("OP_LOOP", -1, chunk, offset);
  }
  case OP_PRINT: {
    return simpleInstruction("OP_PRINT", offset);
  }
  case OP_JUMP: {
    return jumpInstruction("OP_JUMP", 1, chunk, offset);
  }
  case OP_JUMP_IF_FALSE: {
    return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
  }
  case OP_CONSTANT: {
    return constantInstruction("OP_CONSTANT", chunk, offset);
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
    return byteInstruction("OP_GET_LOCAL", chunk, offset);
  }
  case OP_SET_LOCAL: {
    return byteInstruction("OP_SET_LOCAL", chunk, offset);
  }
  case OP_GET_GLOBAL: {
    return constantInstruction("OP_GET_GLOBAL", chunk, offset);
  }
  case OP_DEFINE_GLOBAL: {
    return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
  }
  case OP_SET_GLOBAL: {
    return constantInstruction("OP_SET_GLOBAL", chunk, offset);
  }
  case OP_GET_PROPERTY: {
    return constantInstruction("OP_GET_PROPERTY", chunk, offset);
  }
  case OP_SET_PROPERTY: {
    return constantInstruction("OP_SET_PROPERTY", chunk, offset);
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
    std::cout << "Unknown opcode" << (int)instruction << "\n";
    return offset + 1;
  }
  }
}
