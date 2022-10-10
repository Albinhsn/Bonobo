module Token where

data TokenType = MINUS | IF | ELSE | RETURN | TRUE | FALSE | BANG | EQUALS | NOT_EQUALS | SLASH | ASTERISK | GREATER_T | LESS_T | ILLEGAL | EOF | IDENT | INT | ASSIGN | PLUS | COMMA | SEMICOLON | LPAREN | RPAREN | LBRACE | RBRACE | FUNCTION | LET deriving (Eq, Show)

data Token = Token {typ :: !TokenType, literal :: !String} deriving (Eq, Show)
