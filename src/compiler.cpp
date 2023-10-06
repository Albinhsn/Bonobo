#include "compiler.h"
#include "common.h"
#include "debug.h"
#include "scanner.h"
#include <iostream>
#include <vector>

Compiler *compiler;
Parser *parser;
Scanner *scanner;

static void initCompiler() {
    compiler = new Compiler;
    compiler->enclosing = nullptr;
    compiler->statements = std::vector<Stmt *>();
    compiler->variables = std::vector<Variable *>();
}

static void endCompiler(Compiler *current) {
    compiler = current->enclosing;
    delete (current);
}

static void errorAt(const char *message) {
    if (parser->hadError) {
        return;
    }

    Token *token = parser->current;
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        //
    } else {
        fprintf(stderr, " at '%d'", token->line);
    }

    fprintf(stderr, ": %s\n", message);
    parser->hadError = true;
    exit(1);
}

static void advance() {
    delete (parser->previous);
    parser->previous = parser->current;
    for (;;) {
        parser->current = scanToken(scanner);
        if (parser->current->type != TOKEN_ERROR) {
            break;
        }
        errorAt("error advancing");
    }
}
static void consume(TokenType type, const char *message) {
    if (parser->current->type == type) {
        advance();
        return;
    }

    errorAt(message);
}

static bool match(TokenType type) {
    if (!(parser->current->type == type)) {
        return false;
    }
    advance();
    return true;
}

static VarType getVarType() {
    switch (parser->previous->type) {
    case TOKEN_INT_TYPE: {
        return INT_VAR;
    }
    case TOKEN_DOUBLE_TYPE: {
        return DOUBLE_VAR;
    }
    case TOKEN_BOOL_TYPE: {
        return BOOL_VAR;
    }
    case TOKEN_STR_TYPE: {
        return STR_VAR;
    }
    case TOKEN_ARRAY_TYPE: {
        return ARRAY_VAR;
    }
    case TOKEN_MAP_TYPE: {
        return MAP_VAR;
    }
    case TOKEN_IDENTIFIER: {
        return STRUCT_VAR;
    }
    case TOKEN_NIL: {
        return NIL_VAR;
    }
    default: {
        break;
    }
    }
    debugToken(parser->previous);
    printf("\n");
    errorAt(" Invalid varType");
    exit(1);
}

static Variable *parseVarType(Variable *var) {
    advance();
    var->type = getVarType();
    if (var->type == ARRAY_VAR) {
        ArrayVariable *arrayVar = new ArrayVariable(var->name);
        consume(TOKEN_LEFT_BRACKET, "Need array type");

        Variable *items = new Variable();
        arrayVar->items = parseVarType(items);

        consume(TOKEN_RIGHT_BRACKET, "Need array type");
        return arrayVar;
    } else if (var->type == MAP_VAR) {
        MapVariable *mapVar = new MapVariable(var->name);
        consume(TOKEN_LEFT_BRACKET, "Need array type");

        mapVar->keys = parseVarType(new Variable());

        consume(TOKEN_COMMA, "Need , before map values");

        mapVar->values = parseVarType(new Variable());

        consume(TOKEN_RIGHT_BRACKET, "Need array type");
        return mapVar;
    } else if (var->type == STRUCT_VAR) {
        StructVariable *structVar = new StructVariable(var->name);

        structVar->structName = *parser->previous;
        return structVar;

    } else {
        return var;
    }
}

static Variable *parseVariable() {
    Variable *var = new Variable();

    consume(TOKEN_IDENTIFIER, "Expected identifier for variable");
    var->name = *parser->previous;

    consume(TOKEN_COLON, "Expected ':' after var name");
    var = parseVarType(var);
    return var;
}

static UnaryOp getUnaryType() {
    switch (parser->previous->type) {
    case TOKEN_BANG: {
        return BANG_UNARY;
    }
    case TOKEN_MINUS: {
        return NEG_UNARY;
    }
    default: {
        errorAt("Can't get unary op for this?");
        exit(1);
    }
    }
}

static ComparisonOp getComparisonOp() {
    switch (parser->previous->type) {
    case TOKEN_LESS: {
        return LESS_COMPARISON;
    }
    case TOKEN_LESS_EQUAL: {
        return LESS_EQUAL_COMPARISON;
    }
    case TOKEN_GREATER: {
        return GREATER_COMPARISON;
    }
    case TOKEN_GREATER_EQUAL: {
        return GREATER_EQUAL_COMPARISON;
    }
    case TOKEN_EQUAL_EQUAL: {
        return EQUAL_EQUAL_COMPARISON;
    }
    default: {
        errorAt("unable to get logical type");
        exit(1);
    }
    }
}

static LogicalOp getLogicalOp() {
    if (parser->previous->type == TOKEN_AND) {
        return AND_LOGICAL;
    } else if (parser->previous->type == TOKEN_OR) {
        return OR_LOGICAL;
    }
    errorAt("Unknown logical op?");
    exit(1);
}

static LiteralType getLiteralType() {
    switch (parser->previous->type) {
    case TOKEN_INT_LITERAL: {
        return INT_LITERAL;
    }
    case TOKEN_DOUBLE_LITERAL: {
        return DOUBLE_LITERAL;
    }
    case TOKEN_TRUE: {
        return BOOL_LITERAL;
    }
    case TOKEN_FALSE: {
        return BOOL_LITERAL;
    }
    case TOKEN_STR_LITERAL: {
        return STR_LITERAL;
    }
    default: {
        break;
    }
    }
    errorAt("Unable to get literal type");
    exit(1);
}

static void literal(Expr *&expr) {
    if (expr == nullptr) {
        expr = new LiteralExpr(*parser->previous, getLiteralType());
    } else {
        switch (expr->type) {
        case BINARY_EXPR: {
            BinaryExpr *binaryExpr = (BinaryExpr *)expr;
            literal(binaryExpr->right);
            break;
        }
        case LOGICAL_EXPR: {
            LogicalExpr *logicalExpr = (LogicalExpr *)expr;
            literal(logicalExpr->right);
            break;
        }
        case COMPARISON_EXPR: {
            ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
            literal(comparisonExpr->right);
            break;
        }
        case UNARY_EXPR: {
            UnaryExpr *unaryExpr = (UnaryExpr *)expr;
            literal(unaryExpr->right);
            break;
        }
        case CALL_EXPR: {
            CallExpr *callExpr = (CallExpr *)expr;
            literal(callExpr->arguments.back());
            break;
        }
        default: {
            errorAt("Can't add literal to expr");
            break;
        }
        }
    }
}

static void unary(Expr *&expr) {
    if (expr == nullptr) {
        UnaryExpr *unaryExpr = new UnaryExpr(getUnaryType());

        expr = unaryExpr;
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        unary(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        unary(logicalExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        unary(comparisonExpr->right);
        break;
    }
    default: {
        errorAt("Can't add unary to expr");
    }
    }
}

static void grouping(Expr *&expr) {
    if (expr == nullptr) {
        expr = new GroupingExpr(expression(expr));
        consume(TOKEN_RIGHT_PAREN, "Grouping wasn't closed");

        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        grouping(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        grouping(logicalExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        grouping(comparisonExpr->right);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        grouping(unaryExpr->right);
        break;
    }
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        CallExpr *callExpr = new CallExpr(varExpr->name);
        callExpr->callee = varExpr->name;
        if (!match(TOKEN_RIGHT_PAREN)) {
            while (true) {
                callExpr->arguments.push_back(expression(nullptr));
                if (match(TOKEN_RIGHT_PAREN)) {
                    break;
                }
                consume(TOKEN_COMMA, "Expect ',' after func param");
            }
        }
        expr = callExpr;
        break;
    }
    default: {
        errorAt("Can't add grouping to this?");
    }
    }
}

static BinaryOp getBinaryOp(Token *token) {
    switch (token->type) {
    case TOKEN_PLUS: {
        return ADD;
    }
    case TOKEN_MINUS: {
        return SUB;
    }
    case TOKEN_SLASH: {
        return DIV;
    }
    case TOKEN_STAR: {
        return MUL;
    }
    default: {
        errorAt("Unknown binaryOp type");
        exit(1);
    }
    }
}

static void operation(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("What can't op without expr");
    }
    BinaryOp op = getBinaryOp(parser->previous);
    switch (expr->type) {
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        operation(comparisonExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        operation(logicalExpr->right);
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;

        if (op >= MUL && binaryExpr->op <= SUB) {
            if (binaryExpr->right != nullptr && binaryExpr->right->type == BINARY_EXPR) {
                operation(binaryExpr->right);
            } else {

                binaryExpr->right = new BinaryExpr(binaryExpr->right, op);
                expr = binaryExpr;
            }
        } else {
            expr = new BinaryExpr(binaryExpr, op);
        }
        break;
    }
    default: {
        expr = new BinaryExpr(expr, op);
        break;
    }
    }
}

static void comparison(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("Unable to add logical to empty expr");
    }
    if (expr->type == LOGICAL_EXPR) {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        comparison(logicalExpr->right);
        expr = logicalExpr;

    } else {
        expr = new ComparisonExpr(expr, getComparisonOp());
    }
}

static void identifier(Expr *&expr) {
    if (expr == nullptr) {
        expr = new VarExpr(*parser->previous);
        return;
    }
    switch (expr->type) {
    case INC_EXPR: {
        IncExpr *incExpr = (IncExpr *)expr;
        if (incExpr->expr == nullptr) {
            identifier(incExpr->expr);
            expr = incExpr;
            break;
        }
        errorAt("Can't add identifier to inc expr");
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        identifier(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        identifier(logicalExpr->right);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        identifier(unaryExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        identifier(comparisonExpr->right);
        break;
    }
    default: {
        printf("%d ", expr->type);
        printf("woopsie no identifier yet\n");
        exit(1);
    }
    }
}

static void dot(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("Can't add '.' to nothing?\n");
        return;
    }
    switch (expr->type) {
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        consume(TOKEN_IDENTIFIER, "Expect identifier after '.'");
        DotExpr *dotExpr = new DotExpr(varExpr, *parser->previous);
        expr = dotExpr;
        break;
    }
    case CALL_EXPR: {
        CallExpr *callExpr = (CallExpr *)expr;
        consume(TOKEN_IDENTIFIER, "Expect identifier after '.'");
        DotExpr *dotExpr = new DotExpr(callExpr, *parser->previous);
        expr = dotExpr;
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        identifier(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        identifier(logicalExpr->right);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        identifier(unaryExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        identifier(comparisonExpr->right);
        break;
    }
    default: {
        printf("%d ", expr->type);
        printf("woopsie no dot yet\n");
        exit(1);
    }
    }
}

static bool isChildUnary(Expr *&expr) {
    if (expr == nullptr) {
        return true;
    }
    if (expr->type == BINARY_EXPR) {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        return isChildUnary(binaryExpr->right);
    }

    return false;
}
static bool isEmptyChildUnary(Expr *&expr) {
    if (expr == nullptr) {
        return false;
    } else if (expr->type == UNARY_EXPR) {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        return unaryExpr->right == nullptr ? false : true;
    } else if (expr->type == BINARY_EXPR) {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        return isEmptyChildUnary(binaryExpr->right);
    }

    return false;
}

static void plus(Expr *&expr) {
    if (expr == nullptr) {
        expr = new UnaryExpr(PLUS_UNARY);
        return;
    }
    switch (expr->type) {
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        expr = new IncExpr(nullptr, INC);
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        if (binaryExpr->right == nullptr && binaryExpr->op == ADD && binaryExpr->left->type == VAR_EXPR) {
            expr = new IncExpr(binaryExpr->left, INC);
        } else if (isChildUnary(binaryExpr->right) || isEmptyChildUnary(binaryExpr->right)) {
            plus(binaryExpr->right);
            expr = binaryExpr;
        } else {
            expr = new BinaryExpr(expr, ADD);
        }
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        plus(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        plus(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    default: {
        expr = new BinaryExpr(expr, ADD);
        break;
    }
    }
}
static void minus(Expr *&expr) {
    if (expr == nullptr) {
        expr = new UnaryExpr(NEG_UNARY);
        return;
    }
    switch (expr->type) {
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        expr = new IncExpr(nullptr, DEC);
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        if (binaryExpr->right == nullptr && binaryExpr->op == SUB && binaryExpr->left->type == VAR_EXPR) {
            expr = new IncExpr(binaryExpr->left, DEC);
        } else if (isChildUnary(binaryExpr->right) || isEmptyChildUnary(binaryExpr->right)) {
            minus(binaryExpr->right);
            expr = binaryExpr;
        } else {
            expr = new BinaryExpr(expr, SUB);
        }
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        minus(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        minus(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    default: {
        expr = new BinaryExpr(expr, SUB);
        break;
    }
    }
}

static void index(Expr *&expr) {
    if (expr == nullptr) {
        expr = arrayDeclaration();
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        index(binaryExpr->right);
        expr = binaryExpr;
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        index(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        index(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        index(unaryExpr->right);
        expr = unaryExpr;
        break;
    }
    case INDEX_EXPR: {
        IndexExpr *indexExpr = (IndexExpr *)expr;

        IndexExpr *newIndexExpr = new IndexExpr();
        newIndexExpr->variable = indexExpr;
        newIndexExpr->index = expression(nullptr);

        consume(TOKEN_RIGHT_BRACKET, "Expect ']' after index");
        expr = newIndexExpr;
        break;
    }
    case VAR_EXPR: {
        IndexExpr *indexExpr = new IndexExpr();
        indexExpr->variable = (VarExpr *)expr;
        indexExpr->index = expression(nullptr);
        consume(TOKEN_RIGHT_BRACKET, "Expect ']' after index");

        expr = indexExpr;
        break;
    }
    default: {
        errorAt("Don't know how to index this expr");
    }
    }
}

static void logical(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("Can't add logical op to empty expr?");
    }

    LogicalOp op = getLogicalOp();

    if (expr->type == LOGICAL_EXPR) {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        if (logicalExpr->op < op) {
            logicalExpr->right = new LogicalExpr(logicalExpr->right, op);
            expr = logicalExpr;
        } else {
            expr = new LogicalExpr(logicalExpr, op);
        }
    } else {
        expr = new LogicalExpr(expr, op);
    }
}

static void mapExpression(Expr *&expr) {
    if (expr == nullptr) {
        expr = mapDeclaration();
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        mapExpression(binaryExpr->right);
        expr = binaryExpr;
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        mapExpression(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        mapExpression(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        mapExpression(unaryExpr->right);
        expr = unaryExpr;
        break;
    }
    default: {
        errorAt("Don't know how to parse this expr (mapExpression)");
    }
    }
}

static Expr *expression(Expr *expr) {
    while (parser->current->type != TOKEN_SEMICOLON && parser->current->type != TOKEN_RIGHT_PAREN &&
           parser->current->type != TOKEN_RIGHT_BRACKET && parser->current->type != TOKEN_RIGHT_BRACE &&
           parser->current->type != TOKEN_COLON && parser->current->type != TOKEN_COMMA) {
        advance();

        // debugExpression(expr);
        // printf("\n");
        // debugToken(parser->previous);
        // printf("\n");

        switch (parser->previous->type) {
        case TOKEN_INT_LITERAL: {
            literal(expr);
            break;
        }
        case TOKEN_DOUBLE_LITERAL: {
            literal(expr);
            break;
        }
        case TOKEN_STR_LITERAL: {
            literal(expr);
            break;
        }
        case TOKEN_IDENTIFIER: {
            identifier(expr);
            break;
        }
        case TOKEN_TRUE: {
            literal(expr);
            break;
        }
        case TOKEN_FALSE: {
            literal(expr);
            break;
        }
        case TOKEN_PLUS: {
            plus(expr);
            break;
        }
        case TOKEN_MINUS: {
            minus(expr);
            break;
        }
        case TOKEN_STAR: {
            operation(expr);
            break;
        }
        case TOKEN_SLASH: {
            operation(expr);
            break;
        }
        case TOKEN_BANG: {
            unary(expr);
            break;
        }
        case TOKEN_LESS: {
            comparison(expr);
            break;
        }
        case TOKEN_LESS_EQUAL: {
            comparison(expr);
            break;
        }
        case TOKEN_GREATER: {
            comparison(expr);
            break;
        }
        case TOKEN_GREATER_EQUAL: {
            comparison(expr);
            break;
        }
        case TOKEN_EQUAL_EQUAL: {
            comparison(expr);
            break;
        }
        case TOKEN_LEFT_PAREN: {
            grouping(expr);
            break;
        }
        case TOKEN_LEFT_BRACKET: {
            index(expr);
            break;
        }
        case TOKEN_LEFT_BRACE: {
            mapExpression(expr);
            break;
        }
        case TOKEN_AND: {
            logical(expr);
            break;
        }
        case TOKEN_OR: {
            logical(expr);
            break;
        }
        case TOKEN_DOT: {
            dot(expr);
            break;
        }
        default: {
            errorAt("Can't parse expr with this token");
        }
        }
    }
    return expr;
}

static Expr *arrayDeclaration() {
    ArrayExpr *arrayExpr = new ArrayExpr();
    if (parser->current->type != TOKEN_RIGHT_BRACKET) {
        do {
            arrayExpr->items.push_back(expression(nullptr));
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after array declarations.");
    return arrayExpr;
}

static Expr *mapDeclaration() {
    MapExpr *mapExpr = new MapExpr();
    if (parser->current->type != TOKEN_RIGHT_BRACE) {
        do {
            mapExpr->keys.push_back(expression(nullptr));
            consume(TOKEN_COLON, "Expect colon between key and value");
            mapExpr->values.push_back(expression(nullptr));
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after map items.");
    return mapExpr;
}

static Stmt *varDeclaration() {
    VarStmt *varStmt = new VarStmt;
    varStmt->type = VAR_STMT;
    varStmt->var = parseVariable();
    consume(TOKEN_EQUAL, "Expected assignment at var declaration");

    varStmt->initializer = expression(nullptr);
    // Change this xD
    if (varStmt->initializer->type == ARRAY_EXPR && varStmt->var->type == ARRAY_VAR) {
        ArrayExpr *expr = (ArrayExpr *)varStmt->initializer;
        ArrayVariable *var = (ArrayVariable *)varStmt->var;
        expr->itemType = var->items;
        if (expr->itemType->type == ARRAY_VAR) {
            while (true) {
                if (var->items->type == ARRAY_VAR) {
                    ArrayVariable *arrayVar = (ArrayVariable *)var->items;
                    for (int i = 0; i < expr->items.size(); ++i) {
                        ArrayExpr *item = (ArrayExpr *)expr->items[i];
                        item->itemType = arrayVar->items;
                    }
                    expr = (ArrayExpr *)expr->items.back();
                    var = arrayVar;
                } else {
                    break;
                }
            }
        }
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");

    compiler->variables.push_back(varStmt->var);
    return (Stmt *)varStmt;
}

static Stmt *expressionStatement() {
    if (match(TOKEN_IDENTIFIER)) {
        Token ident = *parser->previous;
        if (match(TOKEN_EQUAL)) {
            AssignStmt *assignStmt = new AssignStmt();
            assignStmt->name = ident;
            assignStmt->value = expression(assignStmt->value);
            return assignStmt;
        } else {
            ExprStmt *exprStmt = new ExprStmt();
            VarExpr *expr = new VarExpr(ident);
            exprStmt->expression = expression(expr);
            return exprStmt;
        }
    } else {
        ExprStmt *exprStmt = new ExprStmt();
        exprStmt->expression = expression(exprStmt->expression);
        return exprStmt;
    }
}

static Stmt *forStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    ForStmt *forStmt = new ForStmt();
    if (match(TOKEN_SEMICOLON)) {
    } else if (match(TOKEN_VAR)) {
        forStmt->initializer = varDeclaration();
    } else {
        forStmt->initializer = expressionStatement();
        consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    }

    if (!match(TOKEN_SEMICOLON)) {
        forStmt->condition = expression(nullptr);
        consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    }
    if (!match(TOKEN_RIGHT_PAREN)) {
        forStmt->increment = expressionStatement();
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
    }

    consume(TOKEN_LEFT_BRACE, "Expect '{' after 'for()'");
    while (!match(TOKEN_RIGHT_BRACE)) {
        forStmt->body.push_back(declaration());
    }
    return forStmt;
}

static Stmt *ifStatement() {
    IfStmt *ifStmt = new IfStmt();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");

    grouping(ifStmt->condition);

    consume(TOKEN_LEFT_BRACE, "Expect '{' after condition.");
    while (!match(TOKEN_RIGHT_BRACE)) {
        ifStmt->thenBranch.push_back(declaration());
    }

    if (match(TOKEN_ELSE)) {
        consume(TOKEN_LEFT_BRACE, "Expect '{' after else");
        while (!match(TOKEN_RIGHT_BRACE)) {
            ifStmt->elseBranch.push_back(declaration());
        }
    }
    return ifStmt;
}

static Stmt *returnStatement() {
    ReturnStmt *returnStmt = new ReturnStmt();
    returnStmt->value = expression(nullptr);
    consume(TOKEN_SEMICOLON, "Expect ';' after expressionStatement");

    return returnStmt;
}

static Stmt *whileStatement() {
    WhileStmt *whileStmt = new WhileStmt();

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    grouping(whileStmt->condition);

    consume(TOKEN_LEFT_BRACE, "Expect '{' after while()");
    while (!match(TOKEN_RIGHT_BRACE)) {
        whileStmt->body.push_back(declaration());
    }
    return whileStmt;
}

static Stmt *structDeclaration() {
    StructStmt *structStmt = new StructStmt();
    consume(TOKEN_IDENTIFIER, "Expect struct name");
    structStmt->name = *parser->previous;
    consume(TOKEN_LEFT_BRACE, "Expect '{' before struct body.");
    while (!match(TOKEN_RIGHT_BRACE)) {
        structStmt->fields.push_back(parseVariable());
        consume(TOKEN_SEMICOLON, "Expect semicolon after struct field identifier");
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after struct end.");
    return structStmt;
}

static Stmt *funDeclaration() {
    FuncStmt *funcStmt = new FuncStmt();
    consume(TOKEN_IDENTIFIER, "Need function name in func declaration");
    funcStmt->name = *parser->previous;

    consume(TOKEN_LEFT_PAREN, "Expect '(' after func name");
    if (!match(TOKEN_RIGHT_PAREN)) {
        funcStmt->params.push_back(parseVariable());
        while (match(TOKEN_COMMA)) {
            funcStmt->params.push_back(parseVariable());
        }
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after func params");
    }

    consume(TOKEN_ARROW, "Expect '->' after func params");
    funcStmt->returnType = parseVarType(new Variable());
    if (funcStmt->returnType->type == STRUCT_VAR) {
        funcStmt->returnType->name = *parser->previous;
    }

    consume(TOKEN_LEFT_BRACE, "Expect '{' after returntype in func declaration");
    while (!match(TOKEN_RIGHT_BRACE)) {
        funcStmt->body.push_back(declaration());
    }
    return funcStmt;
}

static Stmt *statement() {
    if (match(TOKEN_FOR)) {
        return forStatement();
    } else if (match(TOKEN_IF)) {
        return ifStatement();
    } else if (match(TOKEN_RETURN)) {
        return returnStatement();
    } else if (match(TOKEN_WHILE)) {
        return whileStatement();
    } else if (match(TOKEN_BREAK)) {
        BreakStmt *stmt = new BreakStmt();
        consume(TOKEN_SEMICOLON, "Expect ';' after break");
        return stmt;
    } else {
        Stmt *stmt = expressionStatement();
        consume(TOKEN_SEMICOLON, "Expect ';' after expressionStatement");
        return stmt;
    }
}

static Stmt *declaration() {
    if (match(TOKEN_FUN)) {
        return funDeclaration();
    } else if (match(TOKEN_VAR)) {
        return varDeclaration();
    } else if (match(TOKEN_STRUCT_TYPE)) {
        return structDeclaration();
    } else {
        return statement();
    }
}

static void initParser() {
    parser = (Parser *)malloc(sizeof(Parser));
    parser->current = nullptr;
    parser->previous = nullptr;
    parser->hadError = false;
}

Compiler *compile(std::string source) {
    scanner = new Scanner();
    initScanner(scanner, source);

    initParser();

    initCompiler();
    advance();
    while (!match(TOKEN_EOF)) {
        compiler->statements.push_back(declaration());
    }
    bool hadError = parser->hadError;
    // debugStatements(compiler->statements);

    delete (scanner);
    free(parser);

    return compiler;
}
