#include "chunk.h"
#include <cstdlib>

void initChunk(Chunk *chunk) {
  chunk->cp = 0;
  chunk->constP = 0;
}

void writeChunks(Chunk *chunk, uint8_t byte1, uint8_t byte2, int line) {
  chunk->code[chunk->cp] = byte1;
  chunk->lines[chunk->cp] = line;
  chunk->cp++;

  chunk->code[chunk->cp] = byte2;
  chunk->lines[chunk->cp] = line;
  chunk->cp++;
}

void writeChunk(Chunk *chunk, uint8_t byte, int line) {
  chunk->code[chunk->cp] = byte;
  chunk->lines[chunk->cp++] = line;
}

void freeChunk(Chunk *chunk) { initChunk(chunk); }

int addConstant(Chunk *chunk, Value value) {
  chunk->constants[chunk->constP++] = value;
  return chunk->constP - 1;
}
