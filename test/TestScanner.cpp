#include "../src/common.h"
#include "../src/debug.h"
#include "../src/scanner.h"
#include <gtest/gtest.h>

TEST(TestScanner, TestSingleCharTokens) {
    std::string source = "!<>(){}[],.-+;*/:=";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source.c_str());

    std::vector<Token> tokens = {
        (Token){"!", 1, TOKEN_BANG},
        (Token){"<", 1, TOKEN_LESS},
        (Token){">", 1, TOKEN_GREATER},
        (Token){"(", 1, TOKEN_LEFT_PAREN},
        (Token){")", 1, TOKEN_RIGHT_PAREN},
        (Token){"{", 1, TOKEN_LEFT_BRACE},
        (Token){"}", 1, TOKEN_RIGHT_BRACE},
        (Token){"[", 1, TOKEN_LEFT_BRACKET},
        (Token){"]", 1, TOKEN_RIGHT_BRACKET},
        (Token){",", 1, TOKEN_COMMA},
        (Token){".", 1, TOKEN_DOT},
        (Token){"-", 1, TOKEN_MINUS},
        (Token){"+", 1, TOKEN_PLUS},
        (Token){";", 1, TOKEN_SEMICOLON},
        (Token){"*", 1, TOKEN_STAR},
        (Token){"/", 1, TOKEN_SLASH},
        (Token){":", 1, TOKEN_COLON},
        (Token){"=", 1, TOKEN_EQUAL},
        (Token){"EOF", 1, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(scannedToken->lexeme == tokens[i].lexeme);
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestVarInt) {
    std::string source = "var a: int = !5;";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source.c_str());

    std::vector<Token> tokens = {
        (Token){"var", 3, TOKEN_VAR},       (Token){"a", 1, TOKEN_IDENTIFIER},
        (Token){":", 1, TOKEN_COLON},       (Token){"int", 3, TOKEN_INT_TYPE},
        (Token){"=", 1, TOKEN_EQUAL},       (Token){"!", 1, TOKEN_BANG},
        (Token){"5", 1, TOKEN_INT_LITERAL}, (Token){";", 1, TOKEN_SEMICOLON},
        (Token){"EOF", 3, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(scannedToken->lexeme == tokens[i].lexeme);
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestDoubleCharTokens) {
    std::string source = "!=<=>=->";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source.c_str());

    std::vector<Token> tokens = {
        (Token){"!=", 2, TOKEN_BANG_EQUAL},
        (Token){"<=", 2, TOKEN_LESS_EQUAL},
        (Token){">=", 2, TOKEN_GREATER_EQUAL},
        (Token){"->", 2, TOKEN_ARROW},
        (Token){"EOF", 3, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(scannedToken->lexeme == tokens[i].lexeme);
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestLiterals) {
    std::string source =
        "arla haren \"Hello,\"; \" Sailor!\" 1.0 2 20.45 123 a1";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source.c_str());

    std::vector<Token> tokens = {
        (Token){"arla", 4, TOKEN_IDENTIFIER},
        (Token){"haren", 5, TOKEN_IDENTIFIER},
        (Token){"Hello,", 6, TOKEN_STR_LITERAL},
        (Token){";", 1, TOKEN_SEMICOLON},
        (Token){" Sailor!", 8, TOKEN_STR_LITERAL},
        (Token){"1.0", 3, TOKEN_DOUBLE_LITERAL},
        (Token){"2", 1, TOKEN_INT_LITERAL},
        (Token){"20.45", 5, TOKEN_DOUBLE_LITERAL},
        (Token){"123", 3, TOKEN_INT_LITERAL},
        (Token){"a1", 2, TOKEN_IDENTIFIER},
        (Token){"EOF", 3, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(scannedToken->lexeme == tokens[i].lexeme);
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestKeywords) {
    std::string source =
        "struct print else false for fun if nil return true while and or var";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source.c_str());

    std::vector<Token> tokens = {
        (Token){"struct", 6, TOKEN_STRUCT_TYPE},
        (Token){"print", 5, TOKEN_PRINT},
        (Token){"else", 4, TOKEN_ELSE},
        (Token){"false", 5, TOKEN_FALSE},
        (Token){"for", 3, TOKEN_FOR},
        (Token){"fun", 3, TOKEN_FUN},
        (Token){"if", 2, TOKEN_IF},
        (Token){"nil", 3, TOKEN_NIL},
        (Token){"return", 6, TOKEN_RETURN},
        (Token){"true", 4, TOKEN_TRUE},
        (Token){"while", 5, TOKEN_WHILE},
        (Token){"and", 3, TOKEN_AND},
        (Token){"or", 2, TOKEN_OR},
        (Token){"var", 3, TOKEN_VAR},
        (Token){"EOF", 3, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(scannedToken->lexeme == tokens[i].lexeme);
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestFibonnaci) {
    std::string source = "fun fib(a){\n if(a <= 2){\n return 1;\n}\n return "
                         "fib(a-1) + fib(a-2);\n} fib(35);";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source.c_str());

    std::vector<Token> tokens = {
        (Token){"fun", 3, TOKEN_FUN},
        (Token){"fib", 3, TOKEN_IDENTIFIER},
        (Token){"(", 1, TOKEN_LEFT_PAREN},
        (Token){"a", 1, TOKEN_IDENTIFIER},
        (Token){")", 1,  TOKEN_RIGHT_PAREN},
        (Token){"{", 1,  TOKEN_LEFT_BRACE},
        (Token){"if", 2, TOKEN_IF},
        (Token){"(", 1,  TOKEN_LEFT_PAREN},
        (Token){"a", 1,  TOKEN_IDENTIFIER},
        (Token){"<=", 2,  TOKEN_LESS_EQUAL},
        (Token){"2", 1,  TOKEN_INT_LITERAL},
        (Token){")", 1,  TOKEN_RIGHT_PAREN},
        (Token){"{", 1,  TOKEN_LEFT_BRACE},
        (Token){"return", 6,  TOKEN_RETURN},
        (Token){"1", 1, TOKEN_INT_LITERAL},
        (Token){";", 1, TOKEN_SEMICOLON},
        (Token){"}", 1, TOKEN_RIGHT_BRACE},
        (Token){"return", 6, TOKEN_RETURN},
        (Token){"fib", 3, TOKEN_IDENTIFIER},
        (Token){"(", 1, TOKEN_LEFT_PAREN},
        (Token){"a", 1, TOKEN_IDENTIFIER},
        (Token){"-", 1, TOKEN_MINUS},
        (Token){"1", 1, TOKEN_INT_LITERAL},
        (Token){")", 1, TOKEN_RIGHT_PAREN},
        (Token){"+", 1, TOKEN_PLUS},
        (Token){"fib", 3, TOKEN_IDENTIFIER},
        (Token){"(", 1, TOKEN_LEFT_PAREN},
        (Token){"a", 1, TOKEN_IDENTIFIER},
        (Token){"-", 1, TOKEN_MINUS},
        (Token){"2", 1, TOKEN_INT_LITERAL},
        (Token){")", 1, TOKEN_RIGHT_PAREN},
        (Token){";", 1, TOKEN_SEMICOLON},
        (Token){"}", 1, TOKEN_RIGHT_BRACE},
        (Token){"fib", 3, TOKEN_IDENTIFIER},
        (Token){"(", 1, TOKEN_LEFT_PAREN},
        (Token){"35", 2, TOKEN_INT_LITERAL},
        (Token){")", 1, TOKEN_RIGHT_PAREN},
        (Token){";", 1, TOKEN_SEMICOLON},
        (Token){"EOF", 3, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(scannedToken->lexeme == tokens[i].lexeme);
    }
    EXPECT_EQ(scanner->current, source.size());
}
