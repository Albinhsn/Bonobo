#include "common.h"
#include "debug.h"
#include "opcode.h"
#include <cstdio>
#include <fstream>
#include <iostream>
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

int main(int argc, const char *argv[]) {
  if (argc == 2) {
    runFile(argv[1]);
  } else {
    std::fprintf(stderr, "Usage: clox [path]\n");
    exit(64);
  }
  return 0;
}
