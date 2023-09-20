#include "../src/common.h"
#include "../src/scanner.h"
#include <gtest/gtest.h>

TEST(TestScanner, TestSingleCharTokens) {
    std::string source = "!<>(){}[],.-+;*/:=";
    Scanner *scanner = NULL;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    scanner->source = source.c_str();
    scanner->line = 1;
    scanner->current = 0;

    std::vector<Token> tokens = {
        (Token){"!", 1, 1, TOKEN_BANG},
        (Token){"<", 1, 1, TOKEN_LESS},
        (Token){">", 1, 1, TOKEN_GREATER},
        (Token){"(", 1, 1, TOKEN_LEFT_PAREN},
        (Token){")", 1, 1, TOKEN_RIGHT_PAREN},
        (Token){"{", 1, 1, TOKEN_LEFT_BRACE},
        (Token){"}", 1, 1, TOKEN_RIGHT_BRACE},
        (Token){"[", 1, 1, TOKEN_LEFT_BRACKET},
        (Token){"]", 1, 1, TOKEN_RIGHT_BRACKET},
        (Token){",", 1, 1, TOKEN_COMMA},
        (Token){".", 1, 1, TOKEN_DOT},
        (Token){"-", 1, 1, TOKEN_MINUS},
        (Token){"+", 1, 1, TOKEN_PLUS},
        (Token){";", 1, 1, TOKEN_SEMICOLON},
        (Token){"*", 1, 1, TOKEN_STAR},
        (Token){"/", 1, 1, TOKEN_SLASH},
        (Token){":", 1, 1, TOKEN_COLON},
        (Token){"=", 1, 1, TOKEN_EQUAL},
        (Token){"EOF", 3, 1, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(cmpString(scannedToken->lexeme, scannedToken->length,
                              tokens[i].lexeme, tokens[i].length));
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestVarInt) {
    std::string source = "var a: int = !5;";
    Scanner *scanner = NULL;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    scanner->source = source.c_str();
    scanner->line = 1;
    scanner->current = 0;

    std::vector<Token> tokens = {
        (Token){"var", 3, 1, TOKEN_VAR},
        (Token){"a", 1, 1, TOKEN_IDENTIFIER},
        (Token){":", 1, 1, TOKEN_COLON},
        (Token){"int", 3, 1, TOKEN_INT_LITERAL},
        (Token){"=", 1, 1, TOKEN_EQUAL},
        (Token){"!", 1, 1, TOKEN_BANG},
        (Token){"5", 1, 1, TOKEN_INT},
        (Token){";", 1, 1, TOKEN_SEMICOLON},
        (Token){"EOF", 3, 1, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(cmpString(scannedToken->lexeme, scannedToken->length,
                              tokens[i].lexeme, tokens[i].length));
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestDoubleCharTokens) {
    std::string source = "!=<=>=->";
    Scanner *scanner = NULL;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    scanner->source = source.c_str();
    scanner->line = 1;
    scanner->current = 0;

    std::vector<Token> tokens = {
        (Token){"!=", 2, 1, TOKEN_BANG_EQUAL},
        (Token){"<=", 2, 1, TOKEN_LESS_EQUAL},
        (Token){">=", 2, 1, TOKEN_GREATER_EQUAL},
        (Token){"->", 2, 1, TOKEN_ARROW},
        (Token){"EOF", 3, 1, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(cmpString(scannedToken->lexeme, scannedToken->length,
                              tokens[i].lexeme, tokens[i].length));
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestLiterals) {
    std::string source =
        "arla haren \"Hello,\"; \" Sailor!\" 1.0 2 20.45 123 a1";
    Scanner *scanner = NULL;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    scanner->source = source.c_str();
    scanner->line = 1;
    scanner->current = 0;
    std::vector<Token> tokens = {
        (Token){"arla", 4, 1, TOKEN_IDENTIFIER},
        (Token){"haren", 5, 1, TOKEN_IDENTIFIER},
        (Token){"Hello,", 6, 1, TOKEN_STRING},
        (Token){";,", 1, 1, TOKEN_SEMICOLON},
        (Token){" Sailor!", 8, 1, TOKEN_STRING},
        (Token){"1.0", 3, 1, TOKEN_DOUBLE},
        (Token){"2", 1, 1, TOKEN_INT},
        (Token){"20.45", 5, 1, TOKEN_DOUBLE},
        (Token){"123", 3, 1, TOKEN_INT},
        (Token){"a1", 2, 1, TOKEN_IDENTIFIER},
        (Token){"EOF", 3, 1, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(cmpString(scannedToken->lexeme, scannedToken->length,
                              tokens[i].lexeme, tokens[i].length));
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestKeywords) {
    std::string source =
        "struct print else false for fun if nil return true while and or var";
    Scanner *scanner = NULL;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    scanner->source = source.c_str();
    scanner->line = 1;
    scanner->current = 0;
    std::vector<Token> tokens = {
        (Token){"struct", 6, 1, TOKEN_STRUCT},
        (Token){"print", 5, 1, TOKEN_PRINT},
        (Token){"else", 4, 1, TOKEN_ELSE},
        (Token){"false", 5, 1, TOKEN_FALSE},
        (Token){"for", 3, 1, TOKEN_FOR},
        (Token){"fun", 3, 1, TOKEN_FUN},
        (Token){"if", 2, 1, TOKEN_IF},
        (Token){"nil", 3, 1, TOKEN_NIL},
        (Token){"return", 6, 1, TOKEN_RETURN},
        (Token){"true", 4, 1, TOKEN_TRUE},
        (Token){"while", 5, 1, TOKEN_WHILE},
        (Token){"and", 3, 1, TOKEN_AND},
        (Token){"or", 2, 1, TOKEN_OR},
        (Token){"var", 3, 1, TOKEN_VAR},
        (Token){"EOF", 3, 1, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(cmpString(scannedToken->lexeme, scannedToken->length,
                              tokens[i].lexeme, tokens[i].length));
    }
    EXPECT_EQ(scanner->current, source.size());
}

TEST(TestScanner, TestFibonnaci) {
    std::string source = "fun fib(a){\n if(a <= 2){\n return 1;\n}\n return "
                         "fib(a-1) + fib(a-2);\n} fib(35);";
    Scanner *scanner = NULL;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    scanner->source = source.c_str();
    scanner->line = 1;
    scanner->current = 0;
    std::vector<Token> tokens = {
        (Token){"fun", 3, 1, TOKEN_FUN},
        (Token){"fib", 3, 1, TOKEN_IDENTIFIER},
        (Token){"(", 1, 1, TOKEN_LEFT_PAREN},
        (Token){"a", 1, 1, TOKEN_IDENTIFIER},
        (Token){")", 1, 1, TOKEN_RIGHT_PAREN},
        (Token){"{", 1, 1, TOKEN_LEFT_BRACE},
        (Token){"if", 2, 1, TOKEN_IF},
        (Token){"(", 1, 1, TOKEN_LEFT_PAREN},
        (Token){"a", 1, 1, TOKEN_IDENTIFIER},
        (Token){"<=", 2, 1, TOKEN_LESS_EQUAL},
        (Token){"2", 1, 1, TOKEN_INT},
        (Token){")", 1, 1, TOKEN_RIGHT_PAREN},
        (Token){"{", 1, 1, TOKEN_LEFT_BRACE},
        (Token){"return", 6, 1, TOKEN_RETURN},
        (Token){"1", 1, 1, TOKEN_INT},
        (Token){";", 1, 1, TOKEN_SEMICOLON},
        (Token){"}", 1, 1, TOKEN_RIGHT_BRACE},
        (Token){"return", 6, 1, TOKEN_RETURN},
        (Token){"fib", 3, 1, TOKEN_IDENTIFIER},
        (Token){"(", 1, 1, TOKEN_LEFT_PAREN},
        (Token){"a", 1, 1, TOKEN_IDENTIFIER},
        (Token){"-", 1, 1, TOKEN_MINUS},
        (Token){"1", 1, 1, TOKEN_INT},
        (Token){")", 1, 1, TOKEN_RIGHT_PAREN},
        (Token){"+", 1, 1, TOKEN_PLUS},
        (Token){"fib", 3, 1, TOKEN_IDENTIFIER},
        (Token){"(", 1, 1, TOKEN_LEFT_PAREN},
        (Token){"a", 1, 1, TOKEN_IDENTIFIER},
        (Token){"-", 1, 1, TOKEN_MINUS},
        (Token){"2", 1, 1, TOKEN_INT},
        (Token){")", 1, 1, TOKEN_RIGHT_PAREN},
        (Token){";", 1, 1, TOKEN_SEMICOLON},
        (Token){"}", 1, 1, TOKEN_RIGHT_BRACE},
        (Token){"fib", 3, 1, TOKEN_IDENTIFIER},
        (Token){"(", 1, 1, TOKEN_LEFT_PAREN},
        (Token){"35", 2, 1, TOKEN_INT},
        (Token){")", 1, 1, TOKEN_RIGHT_PAREN},
        (Token){";", 1, 1, TOKEN_SEMICOLON},
        (Token){"EOF", 3, 1, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(cmpString(scannedToken->lexeme, scannedToken->length,
                              tokens[i].lexeme, tokens[i].length));
    }
    EXPECT_EQ(scanner->current, source.size());
}
