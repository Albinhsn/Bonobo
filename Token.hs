module Token where

import Data.Map

data TokenType = ILLEGAL | EOF | IDENT | INT | ASSIGN | PLUS | COMMA | SEMICOLON | LPAREN | RPAREN | LBRACE | RBRACE | FUNCTION | LET deriving (Eq, Show)

data Token = Token {typ :: !TokenType, literal :: !String} deriving (Eq, Show)

tokenTypes :: [Token]
tokenTypes =
  [ Token {typ = ILLEGAL, literal = "ILLEGAL"},
    Token {typ = EOF, literal = "EOF"},
    Token {typ = IDENT, literal = "IDENT"},
    Token {typ = INT, literal = "INT"},
    Token {typ = ASSIGN, literal = "="},
    Token {typ = PLUS, literal = "+"},
    Token {typ = COMMA, literal = ","},
    Token {typ = SEMICOLON, literal = ";"},
    Token {typ = LPAREN, literal = "("},
    Token {typ = RPAREN, literal = ")"},
    Token {typ = LBRACE, literal = "{"},
    Token {typ = RBRACE, literal = "}"},
    Token {typ = FUNCTION, literal = "FUNCTION"},
    Token {typ = LET, literal = "LET"}
  ]
