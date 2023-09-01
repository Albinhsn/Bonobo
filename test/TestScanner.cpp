
#include "../src/scanner.h"
#include <gtest/gtest.h>
#include <iostream>
#include <string>
#include <vector>

Token createToken(std::string literal, TokenType type, int line, int indent) {
  Token token;
  token.literal = literal;
  token.type = type;
  token.line = line;
  token.indent = indent;

  return token;
}

TEST(TestScanner, TestSingleCharTokens) {
  std::string source = "!<>(){}[],.-+;*/:=";
  Scanner *scanner = initScanner(source);
  std::vector<Token> tokens = {
      (Token){"!", 1, 0, TOKEN_BANG},
      (Token){"<", 1, 0, TOKEN_LESS},
      (Token){">", 1, 0, TOKEN_GREATER},
      (Token){"(", 1, 0, TOKEN_LEFT_PAREN},
      (Token){")", 1, 0, TOKEN_RIGHT_PAREN},
      (Token){"{", 1, 0, TOKEN_LEFT_BRACE},
      (Token){"}", 1, 0, TOKEN_RIGHT_BRACE},
      (Token){"[", 1, 0, TOKEN_LEFT_BRACKET},
      (Token){"]", 1, 0, TOKEN_RIGHT_BRACKET},
      (Token){",", 1, 0, TOKEN_COMMA},
      (Token){".", 1, 0, TOKEN_DOT},
      (Token){"-", 1, 0, TOKEN_MINUS},
      (Token){"+", 1, 0, TOKEN_PLUS},
      (Token){";", 1, 0, TOKEN_SEMICOLON},
      (Token){"*", 1, 0, TOKEN_STAR},
      (Token){"/", 1, 0, TOKEN_SLASH},
      (Token){":", 1, 0, TOKEN_COLON},
      (Token){"=", 1, 0, TOKEN_EQUAL},
      (Token){"EOF", 1, 0, TOKEN_EOF},
  };
  for (int i = 0; i < tokens.size(); i++) {
    Token *scannedToken = scanToken(scanner);
    EXPECT_EQ(scannedToken->type, tokens[i].type);
    EXPECT_EQ(scannedToken->literal, tokens[i].literal);
  }
  EXPECT_EQ(scanner->current, scanner->source.size());
}

TEST(TestScanner, TestDoubleCharTokens) {
  std::string source = "!=<=>=->";
  Scanner *scanner = initScanner(source);
  std::vector<Token> tokens = {
      (Token){"!=", 1, 0, TOKEN_BANG_EQUAL},
      (Token){"<=", 1, 0, TOKEN_LESS_EQUAL},
      (Token){">=", 1, 0, TOKEN_GREATER_EQUAL},
      (Token){"->", 1, 0, TOKEN_ARROW},
      (Token){"EOF", 1, 0, TOKEN_EOF},
  };
  for (int i = 0; i < tokens.size(); i++) {
    Token *scannedToken = scanToken(scanner);
    EXPECT_EQ(scannedToken->type, tokens[i].type);
    EXPECT_EQ(scannedToken->literal, tokens[i].literal);
  }
  EXPECT_EQ(scanner->current, scanner->source.size());
}

TEST(TestScanner, TestLiterals) {
  std::string source = "arla haren \"Hello,\" \" Sailor!\" 1.0 2 20.45 123";
  Scanner *scanner = initScanner(source);
  std::vector<Token> tokens = {
      (Token){"arla", 1, 0, TOKEN_IDENTIFIER},
      (Token){"haren", 1, 0, TOKEN_IDENTIFIER},
      (Token){"Hello,", 1, 0, TOKEN_STRING},
      (Token){" Sailor!", 1, 0, TOKEN_STRING},
      (Token){"1.0", 1, 0, TOKEN_NUMBER},
      (Token){"2", 1, 0, TOKEN_NUMBER},
      (Token){"20.45", 1, 0, TOKEN_NUMBER},
      (Token){"123", 1, 0, TOKEN_NUMBER},

      (Token){"EOF", 1, 0, TOKEN_EOF},
  };
  for (int i = 0; i < tokens.size(); i++) {
    Token *scannedToken = scanToken(scanner);
    EXPECT_EQ(scannedToken->type, tokens[i].type);
    EXPECT_EQ(scannedToken->literal, tokens[i].literal);
  }
  EXPECT_EQ(scanner->current, scanner->source.size());
}

TEST(TestScanner, TestKeywords) {
  std::string source =
      "struct print else false for fun if nil return true while and or var";
  Scanner *scanner = initScanner(source);
  std::vector<Token> tokens = {
      (Token){"struct", 1, 0, TOKEN_STRUCT},
      (Token){"print", 1, 0, TOKEN_PRINT},
      (Token){"else", 1, 0, TOKEN_ELSE},
      (Token){"false", 1, 0, TOKEN_FALSE},
      (Token){"for", 1, 0, TOKEN_FOR},
      (Token){"fun", 1, 0, TOKEN_FUN},
      (Token){"if", 1, 0, TOKEN_IF},
      (Token){"nil", 1, 0, TOKEN_NIL},
      (Token){"return", 1, 0, TOKEN_RETURN},
      (Token){"true", 1, 0, TOKEN_TRUE},
      (Token){"while", 1, 0, TOKEN_WHILE},
      (Token){"and", 1, 0, TOKEN_AND},
      (Token){"or", 1, 0, TOKEN_OR},
      (Token){"var", 1, 0, TOKEN_VAR},
      (Token){"EOF", 1, 0, TOKEN_EOF},
  };
  for (int i = 0; i < tokens.size(); i++) {
    Token *scannedToken = scanToken(scanner);
    EXPECT_EQ(scannedToken->type, tokens[i].type);
    EXPECT_EQ(scannedToken->literal, tokens[i].literal);
  }
  EXPECT_EQ(scanner->current, scanner->source.size());
}
