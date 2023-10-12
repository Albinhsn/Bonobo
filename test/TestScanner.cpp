#include "../src/common.h"
#include "../src/debug.h"
#include "../src/scanner.h"
#include <gtest/gtest.h>

TEST(TestScanner, TestSingleCharTokens) {
    const char *source = "!<>(){}[],.-+;*/:=";
    Scanner *scanner = new Scanner();
    initScanner(scanner, source);

    std::vector<Token> tokens = {
        (Token){"!", 1, 0, TOKEN_BANG},          (Token){"<", 1, 0, TOKEN_LESS},
        (Token){">", 1, 0, TOKEN_GREATER},       (Token){"(", 1, 0, TOKEN_LEFT_PAREN},
        (Token){")", 1, 0, TOKEN_RIGHT_PAREN},   (Token){"{", 1, 0, TOKEN_LEFT_BRACE},
        (Token){"}", 1, 0, TOKEN_RIGHT_BRACE},   (Token){"[", 1, 0, TOKEN_LEFT_BRACKET},
        (Token){"]", 1, 0, TOKEN_RIGHT_BRACKET}, (Token){",", 1, 0, TOKEN_COMMA},
        (Token){".", 1, 0, TOKEN_DOT},           (Token){"-", 1, 0, TOKEN_MINUS},
        (Token){"+", 1, 0, TOKEN_PLUS},          (Token){";", 1, 0, TOKEN_SEMICOLON},
        (Token){"*", 1, 0, TOKEN_STAR},          (Token){"/", 1, 0, TOKEN_SLASH},
        (Token){":", 1, 0, TOKEN_COLON},         (Token){"=", 1, 0, TOKEN_EQUAL},
        (Token){"EOF", 3, 0, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(std::string(scannedToken->lexeme, scannedToken->length) ==
                    std::string(tokens[i].lexeme, tokens[i].length));
    }
}

TEST(TestScanner, TestVarInt) {
    const char *source = "var a: int = !5;";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source);

    std::vector<Token> tokens = {
        (Token){"var", 3, 0, TOKEN_VAR},       (Token){"a", 1, 0, TOKEN_IDENTIFIER}, (Token){":", 1, 0, TOKEN_COLON},
        (Token){"int", 3, 0, TOKEN_INT_TYPE},  (Token){"=", 1, 0, TOKEN_EQUAL},      (Token){"!", 1, 0, TOKEN_BANG},
        (Token){"5", 1, 0, TOKEN_INT_LITERAL}, (Token){";", 1, 0, TOKEN_SEMICOLON},  (Token){"EOF", 3, 0, TOKEN_EOF},
    };

    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(std::string(scannedToken->lexeme, scannedToken->length) ==
                    std::string(tokens[i].lexeme, tokens[i].length));
    }
}

TEST(TestScanner, TestDoubleCharTokens) {
    const char *source = "!=<=>=->";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source);

    std::vector<Token> tokens = {
        (Token){"!=", 2, 0, TOKEN_BANG_EQUAL},    (Token){"<=", 2, 0, TOKEN_LESS_EQUAL},
        (Token){">=", 2, 0, TOKEN_GREATER_EQUAL}, (Token){"->", 2, 0, TOKEN_ARROW},
        (Token){"EOF", 3, 0, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(std::string(scannedToken->lexeme, scannedToken->length) ==
                    std::string(tokens[i].lexeme, tokens[i].length));
    }
}

TEST(TestScanner, TestLiterals) {
    const char *source = "arla haren \"Hello,\"; \" Sailor!\" 1.0 2 20.45 123 a1";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source);

    std::vector<Token> tokens = {
        (Token){"arla", 4, 0, TOKEN_IDENTIFIER},
        (Token){"haren", 5, 0, TOKEN_IDENTIFIER},
        (Token){"Hello,", 6, 0, TOKEN_STR_LITERAL},
        (Token){";", 1, 0, TOKEN_SEMICOLON},
        (Token){" Sailor!", 8, 0, TOKEN_STR_LITERAL},
        (Token){"1.0", 3, 0, TOKEN_DOUBLE_LITERAL},
        (Token){"2", 1, 0, TOKEN_INT_LITERAL},
        (Token){"20.45", 5, 0, TOKEN_DOUBLE_LITERAL},
        (Token){"123", 3,0, TOKEN_INT_LITERAL},
        (Token){"a1", 2, 0, TOKEN_IDENTIFIER},
        (Token){"EOF", 3, 0, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(std::string(scannedToken->lexeme, scannedToken->length) ==
                    std::string(tokens[i].lexeme, tokens[i].length));
    }
}

TEST(TestScanner, TestKeywords) {
    const char *source = "struct print break else false for fun if nil return true while and or var";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source);

    std::vector<Token> tokens = {
        (Token){"struct", 6, 0, TOKEN_STRUCT_TYPE},
        (Token){"print", 5, 0, TOKEN_PRINT},
        (Token){"break", 5, 0, TOKEN_BREAK},
        (Token){"else", 4, 0, TOKEN_ELSE},
        (Token){"false", 5, 0, TOKEN_FALSE},
        (Token){"for", 3, 0, TOKEN_FOR},
        (Token){"fun", 3, 0, TOKEN_FUN},
        (Token){"if", 2, 0, TOKEN_IF},
        (Token){"nil", 3, 0, TOKEN_NIL},
        (Token){"return", 6, 0, TOKEN_RETURN},
        (Token){"true", 4, 0, TOKEN_TRUE},
        (Token){"while", 5, 0, TOKEN_WHILE},
        (Token){"and", 3, 0, TOKEN_AND},
        (Token){"or", 2, 0, TOKEN_OR},
        (Token){"var", 3, 0, TOKEN_VAR},
        (Token){"EOF", 3, 0, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(std::string(scannedToken->lexeme, scannedToken->length) ==
                    std::string(tokens[i].lexeme, tokens[i].length));
    }
}

TEST(TestScanner, TestFibonnaci) {
    const char *source = "fun fib(a){\n if(a <= 2){\n return 1;\n}\n return "
                         "fib(a-1) + fib(a-2);\n} fib(35);";
    Scanner *scanner = nullptr;
    scanner = (Scanner *)malloc(sizeof(Scanner));
    initScanner(scanner, source);

    std::vector<Token> tokens = {
        (Token){"fun", 3, 0, TOKEN_FUN},        (Token){"fib", 3, 0, TOKEN_IDENTIFIER},
        (Token){"(", 1, 0, TOKEN_LEFT_PAREN},   (Token){"a", 1, 0, TOKEN_IDENTIFIER},
        (Token){")", 1, 0, TOKEN_RIGHT_PAREN},  (Token){"{", 1, 0, TOKEN_LEFT_BRACE},
        (Token){"if", 2, 0, TOKEN_IF},          (Token){"(", 1, 0, TOKEN_LEFT_PAREN},
        (Token){"a", 1, 0, TOKEN_IDENTIFIER},   (Token){"<=", 2, 0, TOKEN_LESS_EQUAL},
        (Token){"2", 1, 0, TOKEN_INT_LITERAL},  (Token){")", 1, 0, TOKEN_RIGHT_PAREN},
        (Token){"{", 1, 0, TOKEN_LEFT_BRACE},   (Token){"return", 6, 0, TOKEN_RETURN},
        (Token){"1", 1, 0, TOKEN_INT_LITERAL},  (Token){";", 1, 0, TOKEN_SEMICOLON},
        (Token){"}", 1, 0, TOKEN_RIGHT_BRACE},  (Token){"return", 6, 0, TOKEN_RETURN},
        (Token){"fib", 3, 0, TOKEN_IDENTIFIER}, (Token){"(", 1, 0, TOKEN_LEFT_PAREN},
        (Token){"a", 1, 0, TOKEN_IDENTIFIER},   (Token){"-", 1, 0, TOKEN_MINUS},
        (Token){"1", 1, 0, TOKEN_INT_LITERAL},  (Token){")", 1, 0, TOKEN_RIGHT_PAREN},
        (Token){"+", 1, 0, TOKEN_PLUS},         (Token){"fib", 3, 0, TOKEN_IDENTIFIER},
        (Token){"(", 1, 0, TOKEN_LEFT_PAREN},   (Token){"a", 1, 0, TOKEN_IDENTIFIER},
        (Token){"-", 1, 0, TOKEN_MINUS},        (Token){"2", 1, 0, TOKEN_INT_LITERAL},
        (Token){")", 1, 0, TOKEN_RIGHT_PAREN},  (Token){";", 1, 0, TOKEN_SEMICOLON},
        (Token){"}", 1, 0, TOKEN_RIGHT_BRACE},  (Token){"fib", 3, 0, TOKEN_IDENTIFIER},
        (Token){"(", 1, 0, TOKEN_LEFT_PAREN},   (Token){"35", 2, 0, TOKEN_INT_LITERAL},
        (Token){")", 1, 0, TOKEN_RIGHT_PAREN},  (Token){";", 1, 0, TOKEN_SEMICOLON},
        (Token){"EOF", 3, 0, TOKEN_EOF},
    };
    for (int i = 0; i < tokens.size(); i++) {
        Token *scannedToken = scanToken(scanner);
        EXPECT_EQ(scannedToken->type, tokens[i].type);
        EXPECT_TRUE(std::string(scannedToken->lexeme, scannedToken->length) ==
                    std::string(tokens[i].lexeme, tokens[i].length));
    }
}
