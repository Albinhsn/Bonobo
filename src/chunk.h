#ifndef cpplox_chunk_h
#define cpplox_chunk_h

#include "common.h"
#include "value.h"

#define CODE_MAX 500
#define CONST_MAX  100

typedef enum {
  OP_CONSTANT,
  OP_PRINT,
  OP_STRUCT,
  OP_STRUCT_ARG,
  OP_ARRAY,
  OP_MAP,
  OP_INDEX,
  OP_JUMP_IF_FALSE,
  OP_JUMP,
  OP_LOOP,
  OP_CALL,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_EQUAL,
  OP_GREATER,
  OP_GREATER_EQUAL,
  OP_LESS,
  OP_LESS_EQUAL,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,
  OP_DEFINE_GLOBAL,
  OP_SET_GLOBAL,
  OP_GET_GLOBAL,
  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_SET_PROPERTY,
  OP_GET_PROPERTY,
  OP_NEGATE,
  OP_RETURN,
} OpCode;

typedef struct {
  uint8_t code[CODE_MAX];
  int lines[CODE_MAX];
  int cp;
  int constP;
  Value constants[CONST_MAX];
} Chunk;

void freeChunk(Chunk *chunk);
void initChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, uint8_t byte, int line);
void writeChunks(Chunk *chunk, uint8_t byte1, uint8_t byte2, int line);

int addConstant(Chunk *chunk, Value value);

#endif
