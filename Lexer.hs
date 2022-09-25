module Lexer where

import Token

getLexer :: String
getLexer = "Lexer"

nextToken :: Token
nextToken = Token {typ = ILLEGAL, literal = "ILLEGAL"}
