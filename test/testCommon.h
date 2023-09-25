#ifndef TEST_COMMON_HEADER
#define TEST_COMMON_HEADER
#include "../src/common.h"
#include "../src/stmt.h"

std::string readFile(std::string path); 

std::string runLLVMBackend(std::vector<Stmt*> result);

#endif
