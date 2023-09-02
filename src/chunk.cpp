#include "chunk.h"
#include <cstdlib>

void initChunk(Chunk *chunk) {
  chunk->cp = 0;
  chunk->constants = std::vector<Value>();
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

void freeChunk(Chunk *chunk) {
  chunk->constants.clear();
  initChunk(chunk);
}

int addConstant(Chunk *chunk, Value value) {
  chunk->constants.push_back(value);
  return chunk->constants.size() - 1;
}
