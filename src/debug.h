#ifndef cpplox_debug_h
#define cpplox_debug_h

#include "opcode.h"
#include "object.h"
#include <string>

void disassembleChunk(ObjFunction *function, std::string name);
int disassembleInstruction(ObjFunction*function, int offset);
std::string translateInstruction(ObjFunction*function);


#endif
