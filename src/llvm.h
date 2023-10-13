#include "debug.h"
#include "expr.h"
#include "stmt.h"
#include "variables.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include <map>

class ExitBlock {
  public:
    ExitBlock *prev;
    llvm::BasicBlock *exitBlock;
    ExitBlock(ExitBlock *prev, llvm::BasicBlock *exitBlock) {
        this->prev = prev;
        this->exitBlock = exitBlock;
    }
};

class LLVMStruct {
  private:
  public:
    llvm::StructType *structType;
    std::vector<std::string> fields;
    LLVMStruct(std::vector<llvm::Type *> fieldTypes, std::string structName, std::vector<std::string> fields,
               llvm::LLVMContext *ctx) {
        this->fields = fields;
        this->structType = llvm::StructType::create(*ctx, fieldTypes, structName);
    }
};

class LLVMFunction {
  private:
  public:
    bool broke;
    LLVMFunction *enclosing;
    std::vector<std::vector<llvm::AllocaInst *>> scopedVariables;
    std::map<std::string, int> functionArguments;
    std::map<llvm::Value *, llvm::Type *> arrayElements;
    llvm::BasicBlock *entryBlock;
    ExitBlock *exitBlock;
    llvm::Function *function;
    llvm::FunctionType *functionType;

    LLVMFunction(LLVMFunction *enclosing, llvm::FunctionType *funcType, std::string name,
                 std::map<std::string, int> funcArgs, llvm::LLVMContext *ctx, llvm::Module *module) {
        this->broke = false;
        this->exitBlock = nullptr;
        this->enclosing = enclosing;
        this->functionType = funcType;
        this->function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name, *module);
        this->scopedVariables = std::vector<std::vector<llvm::AllocaInst *>>(1);
        this->functionArguments = funcArgs;
        this->entryBlock = llvm::BasicBlock::Create(*ctx, "entry", this->function);
    }
};

class LLVMCompiler {
  private:
  public:
    std::vector<llvm::Function *> callableFunctions;
    llvm::LLVMContext *ctx;
    std::map<std::string, llvm::StructType *> internalStructs;
    std::map<std::string, llvm::FunctionCallee> libraryFuncs;
    std::vector<llvm::Function*> internalFuncs;
    llvm::Module *module;
    std::vector<Variable *> variables;
    std::vector<llvm::AllocaInst *> strings;
    std::map<std::string, LLVMStruct *> structs;
};
void initCompiler(std::vector<Variable *> variables);
void compile(std::vector<Stmt *> stmts);
llvm::Value *compileExpression(Expr *expr);
void compileStatement(Stmt *stmt);

llvm::Value *loadIndex(IndexExpr *indexExpr, Variable *&var);
