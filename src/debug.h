#ifndef cpplox_debug_h
#define cpplox_debug_h

#include "opcode.h"
#include "object.h"

void disassembleChunk(ObjFunction *function, const char * name);
int disassembleInstruction(ObjFunction*function, int offset);


#endif
