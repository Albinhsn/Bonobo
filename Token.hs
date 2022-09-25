module Token where

data TokenType = ILLEGAL | EOF | IDENT | INT | ASSIGN | PLUS | COMMA | SEMICOLON | LPAREN | RPAREN | LBRACE | RBRACE | FUNCTION | LET deriving (Eq, Show)

data Token = Token {typ :: !TokenType, literal :: !String} deriving (Eq, Show)

tokenTypes :: [(TokenType, Token)]
tokenTypes =
  [ (ILLEGAL, Token {typ = ILLEGAL, literal = "ILLEGAL"}),
    (EOF, Token {typ = EOF, literal = "EOF"}),
    (IDENT, Token {typ = IDENT, literal = "IDENT"}),
    (INT, Token {typ = INT, literal = "INT"}),
    (ASSIGN, Token {typ = ASSIGN, literal = "="}),
    (PLUS, Token {typ = PLUS, literal = "+"}),
    (COMMA, Token {typ = COMMA, literal = ","}),
    (SEMICOLON, Token {typ = SEMICOLON, literal = ";"}),
    (LPAREN, Token {typ = LPAREN, literal = "("}),
    (RPAREN, Token {typ = RPAREN, literal = ")"}),
    (LBRACE, Token {typ = LBRACE, literal = "{"}),
    (RBRACE, Token {typ = RBRACE, literal = "}"}),
    (FUNCTION, Token {typ = FUNCTION, literal = "FUNCTION"}),
    (LET, Token {typ = FUNCTION, literal = "LET"})
  ]
