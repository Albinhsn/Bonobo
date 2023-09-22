#ifndef VARIABLES_HEADER
#define VARIABLES_HEADER

#include "scanner.h"

enum VarType {
    STR_VAR,
    INT_VAR,
    DOUBLE_VAR,
    BOOL_VAR,
    MAP_VAR,
    ARRAY_VAR,
    STRUCT_VAR
};

class Variable {
  private:
  public:
    Token name;
    VarType type;
  Variable(){
    this->name.lexeme = "0";
  }
};

class ArrayVariable : public Variable {
  private:
  public:
    Variable *items;
    ArrayVariable(Token name) {
        this->name = name;
        this->type = ARRAY_VAR;
    }
};

class MapVariable : public Variable {
  private:
  public:
    Variable *keys;
    Variable *values;
    MapVariable(Token name) {
        this->name = name;
        this->type = MAP_VAR;
    }
};

#endif
