module Token where

data TokenType = 
  FOR | -- 'for', the literal 'for' for defining a for loop
  COLON | -- ':'  
  RBRACKET | -- ']' 
  LBRACKET | -- '['
  STRING | -- '"Hello world"' any string starting and ending with '"' 
  MINUS | -- '-' 
  IF |  -- 'if', the literal 'if' for defining a if statement
  ELSE | -- 'else', the literal 'else' for defining a else statement
  RETURN | -- 'return', the literal 'return' for defining a return statement
  TRUE | -- 'True'
  FALSE | -- 'False'
  BANG | -- '!'
  EQUALS | -- '=='
  NOT_EQUALS | -- '!=' 
  SLASH | -- '/'
  ASTERISK | -- '*' 
  GREATER_T | -- '>'
  LESS_T | -- '<'
  ILLEGAL | -- 'any not known character'
  EOF | -- Marks the end a program
  IDENT | -- 'myIdent' any identifier for variables and functions 
  INT | -- '10' any integer number
  ASSIGN | -- '='
  PLUS | -- '+'
  COMMA | -- ','
  SEMICOLON | -- ';'
  LPAREN | -- '('
  RPAREN | -- ')'
  LBRACE | -- '{'
  RBRACE | -- '}'
  FUNCTION | -- 'fn' the literal 'fn' for defining a function
  LET -- 'let' the literal 'let' for defining a variable 
  deriving (Eq, Show)

data Token = Token {line :: !Int, typ :: !TokenType, literal :: !String} deriving (Eq, Show)


