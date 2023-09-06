#ifndef cpplox_memory_h
#define cpplox_memory_h

#include "compiler.h"
#include "object.h"
#include "scanner.h"
#include "vm.h"

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity)*2)

#define ALLOCATE(type, count)                                                  \
  (type *)reallocate(NULL, 0, sizeof(type) * (count))

#define GROW_ARRAY(type, pointer, oldCount, newCount)                          \
  (type *)reallocate(pointer, sizeof(type) * (oldCount),                       \
                     sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount)                                    \
  reallocate(pointer, sizeof(type) * (oldCount), 0)

void markValue(Value value);
void markObject(Obj* object);
void *reallocate(void * pointer, size_t oldSize, size_t newSize);
void collectGarbage();
void freeParser(Parser *parser);
void freeScanner(Scanner *scanner);
void freeCompiler(Compiler *compiler);
void freeObjects(VM * vm);
Value *freeFrame(VM *vm);

#endif
