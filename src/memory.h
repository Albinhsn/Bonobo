#ifndef cpplox_memory_h
#define cpplox_memory_h

#include "compiler.h"
#include "object.h"
#include "scanner.h"
#include "vm.h"

void freeParser(Parser *parser);
void freeScanner(Scanner *scanner);
void freeCompiler(Compiler *compiler);
void freeObjects();
Value *freeFrame(VM *vm);

#endif
