module Lexer where

import Token

data Lexer = Lexer {input :: !String, position :: !Int, readPosition :: !Int, ch :: !Char} deriving (Show)

--parseStringToToken :: String -> [Token]
--parseStringToToken s = [getToken n | n <- s]

charToString :: Char -> String
charToString c = [c]

createLexer :: String -> Lexer
createLexer s = Lexer {input = s, position = 0, readPosition = 1, ch = head s}

getToken :: (Lexer, Char) -> Token
getToken (lexer, s) = token
  where
    token
      | s == '=' || s == '+' || s == ',' || s == ';' || s == '(' || s == ')' || s == '{' || s == '}' = Token {typ = ASSIGN, literal = charToString s}
      | isLetter s = Token {typ = IDENT, literal = readIdentifier lexer}
      | otherwise = Token {typ = EOF, literal = "EOF"}

isLetter :: Char -> Bool
isLetter c = b
  where
    b
      | 'a' <= c && c >= 'z' || 'A' <= c && c >= 'Z' || c == '_' = True
      | otherwise = False

nextToken :: Lexer -> Token
nextToken lexer = t
  where
    t
      | isLetter (ch lexer) = getToken (lexer, ch lexer)
      | otherwise = Token {typ = ILLEGAL, literal = "ILLEGAL"}

readChar :: Lexer -> Lexer
readChar pl = nl
  where
    nl
      | readPosition pl < length (input pl) =
        Lexer
          { input = input pl,
            position = readPosition pl,
            readPosition = readPosition pl + 1,
            ch = input pl !! readPosition pl
          }
      | otherwise = Lexer {input = input pl, position = readPosition pl, readPosition = readPosition pl + 1, ch = '\r'}

readIdentifier :: Lexer -> String
readIdentifier lexer = " "
