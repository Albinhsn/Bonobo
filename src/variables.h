#ifndef VARIABLES_HEADER
#define VARIABLES_HEADER

#include "scanner.h"

enum VarType { STR_VAR, INT_VAR, DOUBLE_VAR, BOOL_VAR, MAP_VAR, ARRAY_VAR, STRUCT_VAR, NIL_VAR };

class Variable {
  private:
  public:
    std::string name;
    VarType type;
    Variable() { this->name = "never assigned name :)"; }
};

class ArrayVariable : public Variable {
  private:
  public:
    Variable *items;
    ArrayVariable(std::string name) {
        this->name = name;
        this->type = ARRAY_VAR;
        this->items = nullptr;
    }
};

class StructVariable : public Variable {
  private:
  public:
    Token structName;
    StructVariable(std::string name) {
        this->name = name;
        this->type = STRUCT_VAR;
    }
};

class MapVariable : public Variable {
  private:
  public:
    Variable *keys;
    Variable *values;
    MapVariable(std::string name) {
        this->name = name;
        this->type = MAP_VAR;
        this->keys = nullptr;
        this->values = nullptr;
    }
};

#endif
