#include "common.h"
#include "debug.h"
#include "llvm.h"
#include "opcode.h"
#include "gtest/gtest.h"
#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>

static std::string readFile(std::string path) {
  std::ifstream t(path);
  std::stringstream buffer;
  if (t.fail()) {
    std::cout << "file doesn't exist\n";
    exit(1);
  }
  buffer << t.rdbuf();
  t.close();
  return buffer.str();
}

static void runFile(std::string path) { std::string source = readFile(path); }

int main(int argc, const char *argv[]) { return 0; }
