#ifndef VARIABLES_HEADER
#define VARIABLES_HEADER

#include <string>
#include <vector>

enum VarType { FUNC_VAR, STR_VAR, INT_VAR, DOUBLE_VAR, BOOL_VAR, MAP_VAR, ARRAY_VAR, STRUCT_VAR, NIL_VAR };

class Variable {
  private:
  public:
    std::string name;
    VarType type;
    Variable(std::string name = "never assigned name :)") { this->name = name; }
};

class FuncVariable : public Variable {
  private:
  public:
    Variable *returnType;
    std::vector<Variable *> params;
    FuncVariable(std::string name, Variable *returnType, std::vector<Variable *> params) {
        this->name = name;
        this->type = FUNC_VAR;
        this->returnType = returnType;
        this->params = params;
    }
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
    std::string structName;
    std::vector<Variable *> fields;
    StructVariable(std::string name, std::string structName, std::vector<Variable *> fields) {
        this->name = name;
        this->structName = structName;
        this->fields = fields;
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
