#include "common.h"
#include "debug.h"
#include "expr.h"
#include "stmt.h"
#include <llvm/Support/Casting.h>
#include <memory>

class LLVMStruct {
  private:
  public:
    std::string name;
    llvm::StructType *structType;
    std::vector<std::string> fields;
    LLVMStruct(std::string name, llvm::StructType *structType, std::vector<std::string> fields) {
        this->name = name;
        this->fields = fields;
        this->structType = structType;
    }
};

class LLVMFunction {
  private:
  public:
    LLVMFunction *enclosing;
    std::vector<std::vector<llvm::AllocaInst *>> scopedVariables;
    std::map<std::string, int> funcArgs;
    llvm::BasicBlock *entryBlock;
    llvm::Function *function;
    llvm::FunctionType *funcType;

    LLVMFunction(LLVMFunction *enclosing, llvm::FunctionType *funcType, std::string name,
                 std::map<std::string, int> funcArgs, llvm::LLVMContext *ctx, llvm::Module *module) {
        this->enclosing = enclosing;
        this->funcType = funcType;
        this->function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name, *module);
        this->scopedVariables = std::vector<std::vector<llvm::AllocaInst *>>(1);
        this->funcArgs = funcArgs;
        this->entryBlock = llvm::BasicBlock::Create(*ctx, "entry", this->function);
    }
};

class LLVMCompiler {
  private:
    llvm::LLVMContext *ctx;
    std::vector<LLVMStruct *> structs;
    llvm::Module *module;
    std::vector<Stmt *> stmts;
    llvm::IRBuilder<> *builder;
    std::map<std::string, llvm::FunctionCallee> libraryFuncs;
    std::vector<llvm::Function *> callableFunctions;
    LLVMFunction *function;

    void endCompiler() {
        this->builder->CreateRet(this->builder->getInt32(0));

        std::error_code errorCode;
        llvm::raw_fd_ostream outLL("./out.ll", errorCode);
        this->module->print(outLL, nullptr);
    }

    bool nameIsAlreadyDeclared(std::string name) {
        for (int i = 0; i < this->function->scopedVariables.back().size(); i++) {
            if (this->function->scopedVariables.back()[i]->getName().str() == name) {
                return true;
            }
        }
        for (int i = 0; i < this->structs.size(); i++) {
            if (this->structs[i]->name == name) {
                return true;
            }
        }
        for (const auto &[key, value] : this->libraryFuncs) {
            if (key == name) {
                return true;
            }
        }
        for (int i = 0; i < this->callableFunctions.size(); ++i) {
            if (this->callableFunctions[i]->getName().str() == name) {
                return true;
            }
        }

        return false;
    }
    llvm::Type *getVariableLLVMType(Variable *var) {
        if (var == nullptr) {
            printf("Variable is none in getVariableLLVMType\n");
            exit(1);
        }
        switch (var->type) {
        case INT_VAR: {
            return this->builder->getInt32Ty();
        }
        case STR_VAR: {
            return llvm::ArrayType::get(this->builder->getInt8Ty(), 0);
        }
        case DOUBLE_VAR: {
            return this->builder->getDoubleTy();
        }
        case BOOL_VAR: {
            return this->builder->getInt1Ty();
        }
        case ARRAY_VAR: {
            return llvm::ArrayType::get(this->builder->getInt32Ty(), 0);
        }
        case STRUCT_VAR: {
            StructVariable *structVar = (StructVariable *)var;
            for (int i = 0; i < this->structs.size(); i++) {
                if (this->structs[i]->name == structVar->structName.lexeme) {
                    return this->structs[i]->structType;
                }
            }
            printf("trying to return unknown struct '%s'\n", structVar->structName.lexeme.c_str());
            exit(1);
        }
        case NIL_VAR: {
            return this->builder->getVoidTy();
        }
        default: {
            printf("unknown llvmType\n");
            exit(1);
        }
        }
    }
    // llvm::MaybeAlign getAlignmentFromStruct(llvm::StructType *structType) {}

    llvm::MaybeAlign getAlignment(llvm::Type *type) { return llvm::MaybeAlign(type->getPrimitiveSizeInBits() / 8); }

    llvm::Value *lookupVariable(std::string name) {
        if (this->function->enclosing) {
            if (this->function->funcArgs.count(name)) {
                int i = 0, value = this->function->funcArgs[name];
                for (llvm::Function::arg_iterator arg = this->function->function->arg_begin();
                     arg != this->function->function->arg_end(); ++arg) {
                    if (i == value) {
                        return arg;
                    }
                    ++i;
                }
            }
        }
        for (int i = this->function->scopedVariables.size() - 1; i >= 0; i--) {
            std::vector<llvm::AllocaInst *> scopeVars = this->function->scopedVariables[i];

            for (int j = 0; j < scopeVars.size(); ++j) {
                if (scopeVars[j]->getName().str() == name) {
                    return scopeVars[j];
                }
            }
        }
        printf("Unknown variable %s\n", name.c_str());
        exit(1);
    }

    bool checkVariableValueMatch(Variable *var, llvm::Value *&value) {
        if (value->getType() == llvm::Type::getInt32Ty(*this->ctx)) {
            if (var->type == DOUBLE_VAR) {
                value = this->builder->CreateUIToFP(value, this->builder->getDoubleTy());
                return true;
            }
            return var->type == INT_VAR;

        } else if (value->getType() == llvm::Type::getInt1Ty(*this->ctx)) {
            return var->type == BOOL_VAR;

        } else if (value->getType() == llvm::Type::getDoubleTy(*this->ctx)) {
            if (var->type == INT_VAR) {
                value = this->builder->CreateFPToUI(value, this->builder->getInt32Ty());
                return true;
            }
            return var->type == DOUBLE_VAR;
        } else if (value->getType()->isStructTy()) {
            // ToDo this needs to check underlying type as well
            return var->type == STR_VAR || var->type == ARRAY_VAR || var->type == MAP_VAR || var->type == STRUCT_VAR;
        }
        if (value->getType()->isPointerTy()) {
            printf("was pointer\n");
        } else if (value->getType() == llvm::Type::getInt8Ty(*this->ctx)) {
            printf("was int8\n");
        } else {
            printf("unknown value\n");
        }

        return false;
    }

    llvm::Value *compileLiteral(LiteralExpr *expr) {
        std::string stringLiteral = expr->literal.lexeme;
        switch (expr->literalType) {
        case STR_LITERAL: {
            // Create the string
            int size = stringLiteral.size() + 1;
            llvm::Value *str = this->builder->CreateGlobalString(stringLiteral);
            llvm::ArrayType *arrayType = llvm::ArrayType::get(this->builder->getInt8Ty(), size);

            // Create the string struct
            std::vector<llvm::Type *> fieldTypes = {arrayType, this->builder->getInt32Ty()};
            llvm::StructType *structType = llvm::StructType::create(*this->ctx, fieldTypes, "string");
            llvm::AllocaInst *stringInstance = this->builder->CreateAlloca(structType, nullptr, "string");

            // Copy the string into the struct
            llvm::Value *strGep = this->builder->CreateStructGEP(structType, stringInstance, 0);
            this->builder->CreateMemCpy(strGep, llvm::MaybeAlign(1), str, llvm::MaybeAlign(1), size);

            // Store the length in the struct
            strGep = this->builder->CreateStructGEP(structType, stringInstance, 1);
            this->builder->CreateStore(this->builder->getInt32(size), strGep);

            return stringInstance;
        }
        case INT_LITERAL: {
            return this->builder->getInt32(stoi(stringLiteral));
        }
        case BOOL_LITERAL: {
            if (stringLiteral == "true") {
                return this->builder->getInt1(1);
            } else {
                return this->builder->getInt1(0);
            }
        }
        case DOUBLE_LITERAL: {
            return llvm::ConstantFP::get(this->builder->getDoubleTy(), stod(stringLiteral));
        }
        }
        printf("how did we get here?\n");
        exit(1);
    }

    llvm::Value *loadAllocaInst(llvm::Value *value) {
        if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            return this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
        }
        return value;
    }

    llvm::GlobalVariable *createGlobalVariable(llvm::Type *type, llvm::Constant *value) {
        return new llvm::GlobalVariable(*this->module, type, false, llvm::GlobalValue::PrivateLinkage, value);
    }
    bool isStringTy(llvm::Type *type) {
        if (llvm::StructType *structType = llvm::dyn_cast<llvm::StructType>(type)) {
            if (llvm::ArrayType *arrayType = llvm::dyn_cast<llvm::ArrayType>(structType->getStructElementType(0))) {
                return arrayType->getArrayElementType() == llvm::Type::getInt8Ty(*this->ctx);
            }
        }
        return false;
    }

    llvm::Value *getStringArray(llvm::Value *value) {
        llvm::AllocaInst *valueAlloca = llvm::dyn_cast<llvm::AllocaInst>(value);
        llvm::Value *loadedValue = this->builder->CreateLoad(valueAlloca->getAllocatedType(), valueAlloca);
        llvm::StructType *structType = llvm::dyn_cast<llvm::StructType>(loadedValue->getType());
        llvm::Value *valueObjPtr = this->builder->CreateInBoundsGEP(
            valueAlloca->getAllocatedType(), valueAlloca, {this->builder->getInt32(0), this->builder->getInt32(0)});
        return this->builder->CreateLoad(structType->getStructElementType(0), valueObjPtr);
    }
    llvm::Value *concatStrings(llvm::Value *left, llvm::Value *right) {
        llvm::Value *leftArr = getStringArray(left);
        int leftSize = leftArr->getType()->getArrayNumElements();
        llvm::Value *rightArr = getStringArray(right);
        int rightSize = rightArr->getType()->getArrayNumElements();

        llvm::ArrayType *arrayType = llvm::ArrayType::get(this->builder->getInt8Ty(), leftSize + rightSize);

        // Create the string struct
        std::vector<llvm::Type *> fieldTypes = {arrayType, this->builder->getInt32Ty()};
        llvm::StructType *structType = llvm::StructType::create(*this->ctx, fieldTypes, "string");
        llvm::AllocaInst *stringInstance = this->builder->CreateAlloca(structType, nullptr, "string");

        // Copy the string into the struct
        llvm::Value *strGep = this->builder->CreateStructGEP(structType, stringInstance, 0);
        this->builder->CreateMemCpy(strGep, llvm::MaybeAlign(1), leftArr, llvm::MaybeAlign(1), leftSize);

        this->builder->CreateCall(this->libraryFuncs["strcat"], {strGep, rightArr});
        printf("GOT\n");
        // Store the length in the struct
        strGep = this->builder->CreateStructGEP(structType, stringInstance, 1);
        this->builder->CreateStore(this->builder->getInt32(leftSize + rightSize), strGep);

        return stringInstance;
    }

    llvm::Value *compileExpression(Expr *expr) {
        switch (expr->type) {
        case BINARY_EXPR: {
            BinaryExpr *binaryExpr = (BinaryExpr *)expr;

            llvm::Value *leftOrig = compileExpression(binaryExpr->left);
            llvm::Value *left = loadAllocaInst(leftOrig);

            llvm::Value *rightOrig = compileExpression(binaryExpr->right);
            llvm::Value *right = loadAllocaInst(rightOrig);

            llvm::Type *leftType = left->getType();
            llvm::Type *rightType = right->getType();
            if (isStringTy(leftType) && isStringTy(rightType)) {
                return concatStrings(leftOrig, rightOrig);
            }

            // Check that both are literal
            //    Create new LLVMValue after op
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                switch (binaryExpr->op) {
                case ADD: {
                    return this->builder->CreateAdd(left, right);
                }
                case SUB: {
                    return this->builder->CreateSub(left, right);
                }
                case MUL: {
                    return this->builder->CreateMul(left, right);
                }
                case DIV: {
                    return this->builder->CreateUDiv(left, right);
                }
                }
            } else if (leftType->isDoubleTy() && rightType->isDoubleTy()) {
                switch (binaryExpr->op) {
                case ADD: {
                    return this->builder->CreateFAdd(left, right);
                }
                case SUB: {
                    return this->builder->CreateFSub(left, right);
                }
                case MUL: {
                    return this->builder->CreateFMul(left, right);
                }
                case DIV: {
                    return this->builder->CreateFDiv(left, right);
                }
                }

            } else if ((leftType->isIntegerTy() && rightType->isDoubleTy()) ||
                       (leftType->isDoubleTy() && rightType->isIntegerTy())) {
                // Cast the integer
                if (leftType->isIntegerTy()) {
                    left = this->builder->CreateUIToFP(left, this->builder->getDoubleTy());
                } else {
                    right = this->builder->CreateUIToFP(right, this->builder->getDoubleTy());
                }

                switch (binaryExpr->op) {
                case ADD: {
                    return this->builder->CreateFAdd(left, right);
                }
                case SUB: {
                    return this->builder->CreateFSub(left, right);
                }
                case MUL: {
                    return this->builder->CreateFMul(left, right);
                }
                case DIV: {
                    return this->builder->CreateFDiv(left, right);
                }
                }
            }
            printf("Can't do this addition\n");
            exit(1);
        }
        case GROUPING_EXPR: {
            GroupingExpr *groupingExpr = (GroupingExpr *)expr;
            return compileExpression(groupingExpr->expression);
        }
        case LOGICAL_EXPR: {
            LogicalExpr *logicalExpr = (LogicalExpr *)expr;

            llvm::Value *left = compileExpression(logicalExpr->left);
            left = loadAllocaInst(left);

            llvm::Value *right = compileExpression(logicalExpr->right);
            right = loadAllocaInst(right);

            if (left->getType() == this->builder->getInt1Ty() && right->getType() == this->builder->getInt1Ty()) {
                switch (logicalExpr->op) {
                case OR_LOGICAL: {
                    return this->builder->CreateLogicalOr(left, right);
                }
                case AND_LOGICAL: {
                    return this->builder->CreateLogicalAnd(left, right);
                }
                }
            }
            printf("Can't do 'and' or 'or' with these types");
            exit(1);
        }
        case LITERAL_EXPR: {
            return compileLiteral((LiteralExpr *)expr);
        }
        case DOT_EXPR: {
            // ToDo clean this mess up
            DotExpr *dotExpr = (DotExpr *)expr;

            // Check this?
            llvm::Value *value = compileExpression(dotExpr->name);
            if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
                value = this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                if (value->getType()->isStructTy()) {
                    for (int i = 0; i < this->structs.size(); ++i) {
                        LLVMStruct *strukt = this->structs[i];

                        if (strukt->name == allocaInst->getAllocatedType()->getStructName()) {
                            for (int j = 0; j < strukt->fields.size(); j++) {
                                if (strukt->fields[j] == dotExpr->field.lexeme) {
                                    return this->builder->CreateExtractValue(value, j);
                                }
                            }
                            printf("Didn't find property '%s' for struct \n", dotExpr->field.lexeme.c_str());
                            exit(1);
                        }
                    }
                }
            }
            printf("Can't do property lookup on non struct \n");
            exit(1);
        }
        case COMPARISON_EXPR: {
            ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;

            llvm::Value *left = compileExpression(comparisonExpr->left);
            left = loadAllocaInst(left);

            llvm::Value *right = compileExpression(comparisonExpr->right);
            right = loadAllocaInst(right);

            llvm::Type *leftType = left->getType();
            llvm::Type *rightType = right->getType();

            // Need to check fp as well, string equality, array equality,
            // map equality
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {

                switch (comparisonExpr->op) {
                case LESS_EQUAL_COMPARISON: {
                    return this->builder->CreateICmpULE(left, right);
                }
                case LESS_COMPARISON: {
                    return this->builder->CreateICmpULT(left, right);
                }
                case GREATER_COMPARISON: {
                    return this->builder->CreateICmpUGT(left, right);
                }
                case GREATER_EQUAL_COMPARISON: {
                    return this->builder->CreateICmpUGE(left, right);
                }
                case EQUAL_EQUAL_COMPARISON: {
                    return this->builder->CreateICmpEQ(left, right);
                }
                }
            } else if (leftType->isDoubleTy() && rightType->isDoubleTy()) {
                switch (comparisonExpr->op) {
                case LESS_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpULE(left, right);
                }
                case LESS_COMPARISON: {
                    return this->builder->CreateFCmpULT(left, right);
                }
                case GREATER_COMPARISON: {
                    return this->builder->CreateFCmpUGT(left, right);
                }
                case GREATER_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpUGE(left, right);
                }
                case EQUAL_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpOEQ(left, right);
                }
                }
            } else if ((leftType->isIntegerTy() && rightType->isDoubleTy()) ||
                       (leftType->isDoubleTy() && rightType->isIntegerTy())) {
                // Cast the integer
                if (leftType->isIntegerTy()) {
                    left = this->builder->CreateUIToFP(left, this->builder->getDoubleTy());
                } else {
                    right = this->builder->CreateUIToFP(right, this->builder->getDoubleTy());
                }

                switch (comparisonExpr->op) {
                case LESS_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpULE(left, right);
                }
                case LESS_COMPARISON: {
                    return this->builder->CreateFCmpULT(left, right);
                }
                case GREATER_COMPARISON: {
                    return this->builder->CreateFCmpUGT(left, right);
                }
                case GREATER_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpUGE(left, right);
                }
                case EQUAL_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpOEQ(left, right);
                }
                }
            }
            printf("Can't do addition with this?\n");
            exit(1);
        }
        case UNARY_EXPR: {
            UnaryExpr *unaryExpr = (UnaryExpr *)expr;
            llvm::Value *value = compileExpression(unaryExpr->right);
            if (unaryExpr->op == NEG_UNARY) {
                if (value->getType()->isIntegerTy() || value->getType()->isDoubleTy()) {
                    return this->builder->CreateMul(value, this->builder->getInt32(-1));
                }
                printf("Can't do '-' with this type\n");
                exit(1);
            } else if (unaryExpr->op == BANG_UNARY) {
                // Check value type?
                return this->builder->CreateXor(value, 1);
            } else {
                printf("unknown unary expr?\n");
                exit(1);
            }
        }
        case VAR_EXPR: {
            VarExpr *varExpr = (VarExpr *)expr;
            llvm::Value *var = lookupVariable(varExpr->name.lexeme);
            return var;
        }
        case INDEX_EXPR: {
            IndexExpr *indexExpr = (IndexExpr *)expr;
            llvm::Value *variable = compileExpression(indexExpr->variable);
            llvm::Value *index = compileExpression(indexExpr->index);

            if (llvm::AllocaInst *castedVar = llvm::dyn_cast<llvm::AllocaInst>(variable)) {

                if (!index->getType()->isIntegerTy()) {
                    printf("Can't index array with something other then int\n");
                    exit(1);
                }
                llvm::Value *loadedVar = this->builder->CreateLoad(castedVar->getAllocatedType(), castedVar);
                llvm::StructType *structType = llvm::dyn_cast<llvm::StructType>(loadedVar->getType());

                llvm::Value *arrObjPtr = this->builder->CreateInBoundsGEP(
                    castedVar->getAllocatedType(), castedVar, {this->builder->getInt32(0), this->builder->getInt32(0)});
                llvm::Value *arrPtr = this->builder->CreateInBoundsGEP(structType->getStructElementType(0), arrObjPtr,
                                                                       {this->builder->getInt32(0), index});

                return this->builder->CreateLoad(structType->getStructElementType(0)->getArrayElementType(), arrPtr);
            }
            printf("couldn't do it\n");
            exit(1);
        }
        case ARRAY_EXPR: {
            ArrayExpr *arrayExpr = (ArrayExpr *)expr;

            // Compile array items
            std::vector<llvm::Constant *> arrayItems = std::vector<llvm::Constant *>(arrayExpr->items.size());
            for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
                llvm::Value *itemVal = compileExpression(arrayExpr->items[i]);
                // This becomes null when it's a struct?
                arrayItems[i] = llvm::dyn_cast<llvm::Constant>(itemVal);
            }

            // Figure ut which type array has
            llvm::Type *elementType = nullptr;
            if (arrayExpr->itemType == nullptr || arrayExpr->itemType->type == INT_VAR) {
                elementType = this->builder->getInt32Ty();
            } else if (arrayExpr->itemType->type == DOUBLE_VAR) {
                elementType = this->builder->getDoubleTy();
            } else if (arrayExpr->itemType->type == BOOL_VAR) {
                elementType = this->builder->getInt1Ty();
            } else {
                elementType = this->builder->getPtrTy();
                for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
                    if (arrayItems[i] == nullptr) {
                        printf("fucken really?\n");
                    }
                    llvm::AllocaInst *allocInst = llvm::dyn_cast<llvm::AllocaInst>(arrayItems[i]);
                    llvm::Value *strGep = this->builder->CreateStructGEP(allocInst->getType(), allocInst, 0);
                    arrayItems[i] = llvm::dyn_cast<llvm::Constant>(strGep);
                }
            }

            uint64_t arraySize = arrayExpr->items.size();

            llvm::ArrayType *arrayType = llvm::ArrayType::get(elementType, arraySize);

            llvm::Constant *arrayConstant = llvm::ConstantArray::get(arrayType, arrayItems);
            llvm::GlobalVariable *globalArray = createGlobalVariable(arrayType, arrayConstant);

            std::vector<llvm::Type *> fieldTypes = {arrayType, this->builder->getInt32Ty()};

            llvm::StructType *structType = llvm::StructType::create(*this->ctx, fieldTypes, "array");
            llvm::AllocaInst *arrayInstance = this->builder->CreateAlloca(structType, nullptr, "array");

            llvm::Value *strGep = this->builder->CreateStructGEP(structType, arrayInstance, 0);
            llvm::MaybeAlign alignment = getAlignment(elementType);

            this->builder->CreateMemCpy(strGep, alignment, globalArray, alignment, arraySize * alignment->value());
            strGep = this->builder->CreateStructGEP(structType, arrayInstance, 1);
            this->builder->CreateStore(this->builder->getInt32(arraySize), strGep);

            return arrayInstance;
        }
        case MAP_EXPR: {
        }
        case CALL_EXPR: {
            CallExpr *callExpr = (CallExpr *)expr;
            std::string name = callExpr->callee.lexeme;

            for (int i = 0; i < this->structs.size(); ++i) {
                LLVMStruct *strukt = this->structs[i];
                // ToDo implement this
                if (strukt->name == name) {
                    llvm::AllocaInst *struktInstance = builder->CreateAlloca(strukt->structType, nullptr, name);
                    if (callExpr->arguments.size() != strukt->fields.size()) {
                        printf("Strukt has different amount of args, expected: "
                               "%d but got %d",
                               (int)strukt->fields.size(), (int)callExpr->arguments.size());
                        exit(1);
                    }
                    for (int j = 0; j < callExpr->arguments.size(); ++j) {
                        llvm::Value *strGep = this->builder->CreateStructGEP(strukt->structType, struktInstance, j);
                        llvm::Value *paramValue = compileExpression(callExpr->arguments[j]);
                        if (strukt->structType->getContainedType(j) != paramValue->getType()) {
                            printf("Param %d does match it's type\n", j);
                            exit(1);
                        }
                        this->builder->CreateStore(paramValue, strGep);
                    }
                    return this->builder->CreateLoad(strukt->structType, struktInstance);
                }
            }

            std::vector<llvm::Value *> params = std::vector<llvm::Value *>(callExpr->arguments.size());
            for (int i = 0; i < callExpr->arguments.size(); ++i) {
                params[i] = compileExpression(callExpr->arguments[i]);
            }

            llvm::Function *func = nullptr;
            for (int i = 0; i < this->callableFunctions.size(); ++i) {
                if (this->callableFunctions[i]->getName() == name) {
                    func = this->callableFunctions[i];
                    break;
                }
            }
            if (this->function->function->getName() == name) {
                func = this->function->function;
            }
            if (this->libraryFuncs.count(name)) {
                for (int i = 0; i < params.size(); ++i) {
                    if (params[i]->getType()->isPointerTy()) {
                        if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(params[i])) {
                            llvm::Value *structValue =
                                this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                            if (structValue->getType()->isStructTy()) {
                                params[i] =
                                    this->builder->CreateStructGEP(allocaInst->getAllocatedType(), allocaInst, 0);
                            } else {
                                params[i] = structValue;
                            }
                        }
                    }
                }
                return this->builder->CreateCall(this->libraryFuncs[name], params);
            }
            if (func == nullptr) {
                printf("calling unknown func '%s'\n", name.c_str());
                exit(1);
            }
            if (((int)func->arg_size()) != params.size()) {
                printf("Calling %s requires %d params but got %d\n", name.c_str(), (int)func->arg_size(),
                       (int)params.size());
            }
            int i = 0;
            for (llvm::Argument &arg : func->args()) {
                if (arg.getType() != params[i]->getType()) {
                    printf("Invalid arg type in function %s with arg %d\n", name.c_str(), i + 1);
                }
                ++i;
            }
            return this->builder->CreateCall(func, params);
        }
        default: {
            printf("unknown expr\n");
            exit(1);
        }
        }
    }
    void compileStatement(Stmt *stmt) {
        switch (stmt->type) {
        case EXPR_STMT: {
            ExprStmt *exprStmt = (ExprStmt *)stmt;
            compileExpression(exprStmt->expression);
            break;
        }
        case BREAK_STMT: {
            break;
        }
        case ASSIGN_STMT: {
            AssignStmt *assignStmt = (AssignStmt *)stmt;
            llvm::Value *value = compileExpression(assignStmt->value);
            std::string name = assignStmt->name.lexeme;

            llvm::Value *variable = lookupVariable(name);
            // Check type is correct?
            this->builder->CreateStore(value, variable);
            break;
        }
        case RETURN_STMT: {
            ReturnStmt *returnStmt = (ReturnStmt *)stmt;
            // Need to check type is correct;
            if (!this->function->enclosing) {
                printf("Can't return outside of a function\n");
                exit(1);
            }

            llvm::Value *returnValue = compileExpression(returnStmt->value);
            // ToDo  better check for this
            // Check here if it's an allocaInst and then load it before sending
            // it back
            returnValue = loadAllocaInst(returnValue);

            if (this->function->funcType->getReturnType() != returnValue->getType()) {
                printf("funcType -> ");
                debugValueType(this->function->funcType->getReturnType(), this->ctx);
                printf("\n");
                printf("return -> ");
                debugValueType(returnValue->getType(), this->ctx);
                printf("\n");
                printf("Mismatching return in '%s'\n", this->function->function->getName().str().c_str());
                exit(1);
            }

            this->builder->CreateRet(returnValue);
            break;
        }
        case VAR_STMT: {
            VarStmt *varStmt = (VarStmt *)stmt;

            if (nameIsAlreadyDeclared(varStmt->var->name.lexeme)) {
                printf("Can't declare variable '%s', name is already "
                       "declared\n",
                       varStmt->var->name.lexeme.c_str());
                exit(1);
            }
            llvm::Value *value = compileExpression(varStmt->initializer);

            // if (!checkVariableValueMatch(varStmt->var, value)) {
            //     printf("Invalid type mismatch in var declaration\nexpected: ");
            //     debugVariable(varStmt->var);
            //     printf("\nbut got: ");
            //     debugValueType(value->getType(), this->ctx);
            //     printf("\n");
            //     exit(1);
            // }
            if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
                allocaInst->setName(varStmt->var->name.lexeme);
                this->function->scopedVariables.back().push_back(allocaInst);
                break;
            }
            llvm::AllocaInst *allocaInst =
                this->builder->CreateAlloca(value->getType(), nullptr, varStmt->var->name.lexeme);
            this->builder->CreateStore(value, allocaInst);
            this->function->scopedVariables.back().push_back(allocaInst);
            break;
        }
        case WHILE_STMT: {
            WhileStmt *whileStmt = (WhileStmt *)stmt;

            llvm::BasicBlock *loopHeaderBlock =
                llvm::BasicBlock::Create(*this->ctx, "loop.header", this->function->function);

            llvm::BasicBlock *loopBodyBlock =
                llvm::BasicBlock::Create(*this->ctx, "loop.body", this->function->function);

            llvm::BasicBlock *loopExitBlock =
                llvm::BasicBlock::Create(*this->ctx, "loop.exit", this->function->function);

            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            llvm::Value *condition = compileExpression(whileStmt->condition);

            this->builder->CreateCondBr(condition, loopBodyBlock, loopExitBlock);
            this->builder->SetInsertPoint(loopBodyBlock);
            bool broke = false;
            for (int i = 0; i < whileStmt->body.size(); ++i) {
                if (whileStmt->body[i]->type == BREAK_STMT) {
                    broke = true;
                    break;
                }
                compileStatement(whileStmt->body[i]);
            }
            if (!broke) {
                this->builder->CreateBr(loopHeaderBlock);
            }
            this->builder->SetInsertPoint(loopExitBlock);

            break;
        }
        case FOR_STMT: {
            ForStmt *forStmt = (ForStmt *)stmt;

            llvm::BasicBlock *loopHeaderBlock =
                llvm::BasicBlock::Create(*this->ctx, "loop.header", this->function->function);

            llvm::BasicBlock *loopBodyBlock =
                llvm::BasicBlock::Create(*this->ctx, "loop.body", this->function->function);

            llvm::BasicBlock *loopExitBlock =
                llvm::BasicBlock::Create(*this->ctx, "loop.exit", this->function->function);

            compileStatement(forStmt->initializer);
            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            llvm::Value *condition = compileExpression(forStmt->condition);

            this->builder->CreateCondBr(condition, loopBodyBlock, loopExitBlock);
            this->builder->SetInsertPoint(loopBodyBlock);
            bool broke = false;
            for (int i = 0; i < forStmt->body.size(); ++i) {
                if (forStmt->body[i]->type == BREAK_STMT) {
                    this->builder->CreateBr(loopExitBlock);
                    broke = true;
                    break;
                }
                compileStatement(forStmt->body[i]);
            }
            if (!broke) {
                compileStatement(forStmt->increment);
                this->builder->CreateBr(loopHeaderBlock);
            }
            this->builder->SetInsertPoint(loopExitBlock);
            break;
        }
        case STRUCT_STMT: {
            StructStmt *structStmt = (StructStmt *)stmt;
            if (nameIsAlreadyDeclared(structStmt->name.lexeme)) {
                printf("Can't declare struct '%s', name is already declared\n", structStmt->name.lexeme.c_str());
                exit(1);
            }

            std::vector<llvm::Type *> fieldTypes = std::vector<llvm::Type *>(structStmt->fields.size());
            std::vector<std::string> fieldNames = std::vector<std::string>(structStmt->fields.size());

            for (int i = 0; i < fieldTypes.size(); ++i) {
                fieldTypes[i] = getVariableLLVMType(structStmt->fields[i]);
                fieldNames[i] = structStmt->fields[i]->name.lexeme;
            }

            llvm::StructType *structType = llvm::StructType::create(*this->ctx, fieldTypes, structStmt->name.lexeme);
            this->structs.push_back(new LLVMStruct(structStmt->name.lexeme, structType, fieldNames));
            break;
        }
        case IF_STMT: {
            IfStmt *ifStmt = (IfStmt *)stmt;
            llvm::Value *condition = compileExpression(ifStmt->condition);

            llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*ctx, "then", this->function->function);
            llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(*ctx, "else", this->function->function);
            llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*ctx, "merge", this->function->function);

            this->builder->CreateCondBr(condition, thenBlock, elseBlock);
            this->builder->SetInsertPoint(thenBlock);

            this->function->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());
            // This is just bad, especially for multiple returns?
            bool returned = false;
            for (int i = 0; i < ifStmt->thenBranch.size(); ++i) {
                if (ifStmt->thenBranch[i]->type == BREAK_STMT) {
                    break;
                }
                compileStatement(ifStmt->thenBranch[i]);
                if (ifStmt->thenBranch[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
            this->function->scopedVariables.pop_back();
            if (!returned) {
                this->builder->CreateBr(mergeBlock);
            }
            this->builder->SetInsertPoint(elseBlock);

            returned = false;
            this->function->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());
            for (int i = 0; i < ifStmt->elseBranch.size(); ++i) {
                compileStatement(ifStmt->elseBranch[i]);
                if (ifStmt->elseBranch[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
            this->function->scopedVariables.pop_back();
            if (!returned) {
                this->builder->CreateBr(mergeBlock);
            }
            this->builder->SetInsertPoint(mergeBlock);
            break;
        }
        case FUNC_STMT: {
            FuncStmt *funcStmt = (FuncStmt *)stmt;

            if (nameIsAlreadyDeclared(funcStmt->name.lexeme)) {
                printf("Can't declare function '%s', name is already "
                       "declared\n",
                       funcStmt->name.lexeme.c_str());
                exit(1);
            }

            // Fix params
            std::vector<llvm::Type *> params = std::vector<llvm::Type *>(funcStmt->params.size());
            std::map<std::string, int> funcArgs;
            for (int i = 0; i < funcStmt->params.size(); ++i) {
                params[i] = getVariableLLVMType(funcStmt->params[i]);
                funcArgs[funcStmt->params[i]->name.lexeme] = i;
            }

            // Ret type
            llvm::Type *returnType = getVariableLLVMType(funcStmt->returnType);

            // Fix func type
            llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, params, false);

            if (this->function->enclosing) {
                printf("Can't declare a function in a function\n");
                exit(1);
            }

            this->function =
                new LLVMFunction(this->function, funcType, funcStmt->name.lexeme, funcArgs, this->ctx, this->module);
            llvm::IRBuilder<> *prevBuilder = this->builder;
            this->builder = new llvm::IRBuilder<>(this->function->entryBlock);
            bool returned = false;
            for (int i = 0; i < funcStmt->body.size(); ++i) {
                compileStatement(funcStmt->body[i]);
                if (funcStmt->body[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
            if (!returned) {
                if (!this->function->funcType->getReturnType()->isVoidTy()) {
                    printf("Non-void function does not return a value in "
                           "'%s'\n",
                           funcStmt->name.lexeme.c_str());
                    exit(1);
                } else {
                    this->builder->CreateRetVoid();
                }
            }
            this->callableFunctions.push_back(this->function->function);
            this->function = this->function->enclosing;
            this->builder = prevBuilder;
            break;
        }
        }
    }
    std::map<std::string, llvm::FunctionCallee> addLibraryFuncs() {
        std::map<std::string, llvm::FunctionCallee> libraryFuncs = std::map<std::string, llvm::FunctionCallee>();

        std::vector<llvm::Type *> printfArgs = {llvm::Type::getInt8PtrTy(*this->ctx)};
        llvm::FunctionType *printfType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*this->ctx), printfArgs, true);
        llvm::FunctionCallee printfFunc = this->module->getOrInsertFunction("printf", printfType);

        libraryFuncs["printf"] = printfFunc;

        std::vector<llvm::Type *> strcatArgs = {llvm::Type::getInt8PtrTy(*this->ctx),
                                                llvm::Type::getInt8PtrTy(*this->ctx)};
        llvm::FunctionType *strcatType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*this->ctx), strcatArgs, true);
        llvm::FunctionCallee strcatFunc = this->module->getOrInsertFunction("printf", strcatType);

        libraryFuncs["strcat"] = strcatFunc;

        return libraryFuncs;
    }

  public:
    LLVMCompiler(std::vector<Stmt *> stmts) {
        this->stmts = stmts;
        this->ctx = new llvm::LLVMContext();
        this->module = new llvm::Module("Bonobo", *ctx);
        this->callableFunctions = std::vector<llvm::Function *>();
        llvm::FunctionType *funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
        this->function = new LLVMFunction(nullptr, funcType, "main", {}, this->ctx, this->module);

        this->structs = std::vector<LLVMStruct *>();
        this->libraryFuncs = addLibraryFuncs();
        this->builder = new llvm::IRBuilder<>(this->function->entryBlock);
    }
    void compile() {
        for (int i = 0; i < stmts.size(); i++) {
            compileStatement(stmts[i]);
        }
        endCompiler();
    }
};
