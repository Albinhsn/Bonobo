#include "debug.h"

void debugValueType(llvm::Type *type, llvm::LLVMContext *ctx) {
    if (type == nullptr) {
        printf("nullptr");
    } else if (type == llvm::Type::getInt32Ty(*ctx)) {
        printf("int32");
    } else if (type == llvm::Type::getInt1Ty(*ctx)) {
        printf("int1");
    } else if (type == llvm::Type::getDoubleTy(*ctx)) {
        printf("double");
    } else if (type->isPointerTy()) {
        printf("ptr");
    } else if (type->isArrayTy()) {
        printf("array");
    } else if (type->isStructTy()) {
        printf("%s struct", type->getStructName().str().c_str());
    } else {
        printf("unknown type");
    }
}

void debugExpression(Expr *expr) {
    if (expr == nullptr) {
        printf("<null expr>");
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        debugExpression(binaryExpr->left);
        switch (binaryExpr->op) {
        case ADD: {
            printf(" + ");
            break;
        }
        case SUB: {
            printf(" - ");
            break;
        }
        case MUL: {
            printf(" * ");
            break;
        }
        case DIV: {
            printf(" / ");
            break;
        }
        }
        debugExpression(binaryExpr->right);
        break;
    }
    case GROUPING_EXPR: {
        GroupingExpr *groupingExpr = (GroupingExpr *)expr;
        printf("(");
        debugExpression(groupingExpr->expression);
        printf(")");
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        debugExpression(logicalExpr->left);
        if (logicalExpr->op == AND_LOGICAL) {
            printf(" and ");
        } else {
            printf(" or ");
        }
        debugExpression(logicalExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        debugExpression(comparisonExpr->left);
        switch (comparisonExpr->op) {
        case LESS_COMPARISON: {
            printf(" < ");
            break;
        }
        case LESS_EQUAL_COMPARISON: {
            printf(" <= ");
            break;
        }
        case GREATER_COMPARISON: {
            printf(" > ");
            break;
        }
        case GREATER_EQUAL_COMPARISON: {
            printf(" <= ");
            break;
        }
        case EQUAL_EQUAL_COMPARISON: {
            printf(" == ");
            break;
        }
        }
        debugExpression(comparisonExpr->right);
        break;
    }
    case LITERAL_EXPR: {
        LiteralExpr *literalExpr = (LiteralExpr *)expr;
        if (literalExpr->literalType == STR_LITERAL) {
            printf("\"%s\"", literalExpr->literal.c_str());
        } else {
            printf("%s", literalExpr->literal.c_str());
        }
        break;
    }
    case INC_EXPR: {
        IncExpr *incExpr = (IncExpr *)expr;
        if (incExpr->op == INC) {
            printf("++");
        }
        if (incExpr->op == DEC) {
            printf("--");
        }
        debugExpression(incExpr->expr);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        if (unaryExpr->op == BANG_UNARY) {
            printf("!");
        } else if (unaryExpr->op == NEG_UNARY) {
            printf("-");
        } else {
            printf("+");
        }
        debugExpression(unaryExpr->right);
        break;
    }
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        printf("%s", varExpr->name.c_str());
        break;
    }
    case CALL_EXPR: {
        CallExpr *callExpr = (CallExpr *)expr;
        printf("%s(", callExpr->callee.c_str());
        for (int i = 0; i < callExpr->arguments.size(); i++) {
            debugExpression(callExpr->arguments[i]);
            if (i < callExpr->arguments.size() - 1) {
                printf(",");
            }
        }
        printf(")");
        break;
    }
    case ARRAY_EXPR: {
        ArrayExpr *arrayExpr = (ArrayExpr *)expr;
        printf("[");
        for (int i = 0; i < arrayExpr->items.size(); i++) {
            debugExpression(arrayExpr->items[i]);
            if (i < arrayExpr->items.size() - 1) {
                printf(",");
            }
        }
        printf("]");
        break;
    }
    case INDEX_EXPR: {
        IndexExpr *indexExpr = (IndexExpr *)expr;
        debugExpression(indexExpr->variable);
        printf("[");
        debugExpression(indexExpr->index);
        printf("]");
        break;
    }
    case MAP_EXPR: {
        MapExpr *mapExpr = (MapExpr *)expr;

        printf("{");
        for (int i = 0; i < mapExpr->keys.size(); i++) {
            debugExpression(mapExpr->keys[i]);
            printf(":");
            debugExpression(mapExpr->values[i]);
            if (i < mapExpr->keys.size() - 1) {
                printf(", ");
            }
        }
        printf("}");
        break;
    }
    default: {
        printf("unknown expr");
    }
    }
}
const char *debugVarType(VarType varType) {
    switch (varType) {
    case ARRAY_VAR: {
        return "arr";
    }
    case BOOL_VAR: {
        return "bool";
    }
    case INT_VAR: {
        return "int";
    }
    case DOUBLE_VAR: {
        return "double";
    }
    case MAP_VAR: {
        return "map";
    }
    case STRUCT_VAR: {
        return "struct";
    }
    case STR_VAR: {
        return "string";
    }
    default: {
        return "unknown";
    }
    }
}

void debugVariable(Variable *var) {
    if(var == nullptr){
      printf("nullptr?");
      return;
  }
    if (var->name != "never assigned name :)") {
        printf("%s:", var->name.c_str());
    }
    switch (var->type) {
    case ARRAY_VAR: {
        ArrayVariable *arrayVar = (ArrayVariable *)var;
        printf("arr");
        printf("[");
        debugVariable(arrayVar->items);
        printf("]");
        break;
    }
    case MAP_VAR: {
        MapVariable *mapVar = (MapVariable *)var;
        printf("map");
        printf("[");
        debugVariable(mapVar->keys);
        printf(",");
        debugVariable(mapVar->values);
        printf("]");
        break;
    }
    case STRUCT_VAR: {
        StructVariable *structVar = (StructVariable *)var;
        printf("struct '%s'", structVar->structName.c_str());
    }
    default: {
        printf("%s", debugVarType(var->type));
    }
    }
}

void debugStatement(Stmt *statement) {
    switch (statement->type) {
    case COMP_ASSIGN_STMT: {
        CompAssignStmt *compStmt = (CompAssignStmt *)statement;
        printf("%s ", compStmt->name.c_str());
        switch (compStmt->op) {
        case ADD: {
            printf("+");
            break;
        }
        case SUB: {
            printf("-");
            break;
        }
        case MUL: {
            printf("*");
            break;
        }
        case DIV: {
            printf("/");
            break;
        }
        }
        printf(" = ");
        debugExpression(compStmt->right);
        printf(";\n");
    }
    case VAR_STMT: {
        VarStmt *varStmt = (VarStmt *)statement;
        printf("var ");
        debugVariable(varStmt->var);
        printf(" = ");
        debugExpression(varStmt->initializer);
        printf(";");
        break;
    }
    case BREAK_STMT: {
        printf("break;");
        break;
    }
    case FOR_STMT: {
        ForStmt *forStmt = (ForStmt *)statement;
        printf("for(");
        if (forStmt->initializer == nullptr) {
            printf(";");
        } else {
            debugStatement(forStmt->initializer);
        }
        if (forStmt->condition == nullptr) {
            printf(";");
        } else {
            debugExpression(forStmt->condition);
            printf(";");
        }
        if (forStmt->increment != nullptr) {
            debugStatement(forStmt->increment);
        }
        printf(")\n{\n");
        debugStatements(forStmt->body);
        printf("}\n");
        break;
    }
    case STRUCT_STMT: {
        StructStmt *structStmt = (StructStmt *)statement;
        printf("struct %s\n{\n", structStmt->name.c_str());
        for (int i = 0; i < structStmt->fields.size(); i++) {
            debugVariable(structStmt->fields[i]);
            printf(";\n");
        }
        printf("}\n");
        break;
    }
    case RETURN_STMT: {
        ReturnStmt *returnStmt = (ReturnStmt *)statement;
        printf("return ");
        debugExpression(returnStmt->value);
        printf(";");
        break;
    }
    case WHILE_STMT: {
        WhileStmt *whileStmt = (WhileStmt *)statement;
        printf("while");
        debugExpression(whileStmt->condition);
        printf("\n{\n");
        debugStatements(whileStmt->body);
        printf("}\n");
        break;
    }
    case IF_STMT: {
        IfStmt *ifStmt = (IfStmt *)statement;
        printf("if");
        debugExpression(ifStmt->condition);
        printf("\n{\n");
        debugStatements(ifStmt->thenBranch);
        printf("}");
        if (ifStmt->elseBranch.size()) {
            printf("else\n{\n");
            debugStatements(ifStmt->elseBranch);
            printf("}");
        }
        break;
    }
    case FUNC_STMT: {
        FuncStmt *funcStmt = (FuncStmt *)statement;
        printf("fun %s(", funcStmt->name.c_str());
        for (int i = 0; i < funcStmt->params.size(); i++) {
            debugVariable(funcStmt->params[i]);
            if (i != funcStmt->params.size() - 1) {
                printf(",");
            }
        }
        printf(") -> ");
        debugVariable(funcStmt->returnType);
        printf("\n{\n");
        debugStatements(funcStmt->body);
        printf("}\n");

        break;
    }
    case ASSIGN_STMT: {
        AssignStmt *assignStmt = (AssignStmt *)statement;
        debugExpression(assignStmt->variable);
        printf(" = ");
        debugExpression(assignStmt->value);
        printf(";");
        break;
    }
    case EXPR_STMT: {
        ExprStmt *exprStmt = (ExprStmt *)statement;
        debugExpression(exprStmt->expression);
        printf(";");
        break;
    }
    }
}

void debugStatements(std::vector<Stmt *> statements) {
    for (int i = 0; i < statements.size(); i++) {
        debugStatement(statements[i]);
        printf("\n");
    }
}

void debugToken(Token *token) {
    switch (token->type) {
    case TOKEN_LEFT_PAREN: {
        printf("TOKEN_LEFT_PAREN");
        break;
    }
    case TOKEN_RIGHT_PAREN: {
        printf("TOKEN_RIGHT_PAREN");
        break;
    }
    case TOKEN_LEFT_BRACE: {
        printf("TOKEN_LEFT_BRACE");
        break;
    }
    case TOKEN_RIGHT_BRACE: {
        printf("TOKEN_RIGHT_BRACE");
        break;
    }
    case TOKEN_LEFT_BRACKET: {
        printf("TOKEN_LEFT_BRACKET");
        break;
    }
    case TOKEN_RIGHT_BRACKET: {
        printf("TOKEN_RIGHT_BRACKET");
        break;
    }
    case TOKEN_COMMA: {
        printf("TOKEN_COMMA");
        break;
    }
    case TOKEN_DOT: {
        printf("TOKEN_DOT");
        break;
    }
    case TOKEN_MINUS: {
        printf("TOKEN_MINUS");
        break;
    }
    case TOKEN_PLUS: {
        printf("TOKEN_PLUS");
        break;
    }
    case TOKEN_SEMICOLON: {
        printf("TOKEN_SEMICOLON");
        break;
    }
    case TOKEN_SLASH: {
        printf("TOKEN_SLASH");
        break;
    }
    case TOKEN_STAR: {
        printf("TOKEN_STAR");
        break;
    }
    case TOKEN_COLON: {
        printf("TOKEN_COLON");
        break;
    }
    case TOKEN_BANG: {
        printf("TOKEN_BANG");
        break;
    }
    case TOKEN_BANG_EQUAL: {
        printf("TOKEN_BANG_EQUAL");
        break;
    }
    case TOKEN_EQUAL: {
        printf("TOKEN_BANG_EQUAL");
        break;
    }
    case TOKEN_EQUAL_EQUAL: {
        printf("TOKEN_EQUAL_EQUAL");
        break;
    }
    case TOKEN_GREATER: {
        printf("TOKEN_GREATER");
        break;
    }
    case TOKEN_GREATER_EQUAL: {
        printf("TOKEN_GREATER_EQUAL");
        break;
    }
    case TOKEN_LESS: {
        printf("TOKEN_LESS");
        break;
    }
    case TOKEN_LESS_EQUAL: {
        printf("TOKEN_LESS_EQUAL");
        break;
    }
    case TOKEN_ARROW: {
        printf("TOKEN_ARROW");
        break;
    }
    case TOKEN_IDENTIFIER: {
        printf("TOKEN_IDENTIFIER");
        break;
    }
    case TOKEN_INT_LITERAL: {
        printf("TOKEN_INT_LITERAL");
        break;
    }
    case TOKEN_DOUBLE_LITERAL: {
        printf("TOKEN_DOUBLE_LITERAL");
        break;
    }
    case TOKEN_STR_LITERAL: {
        printf("TOKEN_STR_LITERAL");
        break;
    }
    case TOKEN_STR_TYPE: {
        printf("TOKEN_STRING_TYPE");
        break;
    }
    case TOKEN_INT_TYPE: {
        printf("TOKEN_INT_TYPE");
        break;
    }
    case TOKEN_DOUBLE_TYPE: {
        printf("TOKEN_DOUBLE_TYPE");
        break;
    }
    case TOKEN_BOOL_TYPE: {
        printf("TOKEN_BOOL_TYPE");
        break;
    }
    case TOKEN_MAP_TYPE: {
        printf("TOKEN_MAP");
        break;
    }
    case TOKEN_ARRAY_TYPE: {
        printf("TOKEN_ARRAY");
        break;
    }
    case TOKEN_NIL: {
        printf("TOKEN_NIL");
        break;
    }
    case TOKEN_STRUCT_TYPE: {
        printf("TOKEN_STRUCT");
        break;
    }
    case TOKEN_PRINT: {
        printf("TOKEN_PRINT");
        break;
    }
    case TOKEN_ELSE: {
        printf("TOKEN_ELSE");
        break;
    }
    case TOKEN_FALSE: {
        printf("TOKEN_FALSE");
        break;
    }
    case TOKEN_FOR: {
        printf("TOKEN_FOR");
        break;
    }
    case TOKEN_FUN: {
        printf("TOKEN_FUN");
        break;
    }
    case TOKEN_IF: {
        printf("TOKEN_IF");
        break;
    }
    case TOKEN_RETURN: {
        printf("TOKEN_RETURN");
        break;
    }
    case TOKEN_TRUE: {
        printf("TOKEN_TRUE");
        break;
    }
    case TOKEN_WHILE: {
        printf("TOKEN_WHILE");
        break;
    }
    case TOKEN_AND: {
        printf("TOKEN_AND");
        break;
    }
    case TOKEN_OR: {
        printf("TOKEN_OR");
        break;
    }
    case TOKEN_VAR: {
        printf("TOKEN_VAR");
        break;
    }
    case TOKEN_BREAK: {
        printf("TOKEN_BREAK");
        break;
    }
    case TOKEN_ERROR: {
        printf("TOKEN_ERROR");
        break;
    }
    case TOKEN_EOF: {
        printf("TOKEN_EOF");
        break;
    }
    }
}
