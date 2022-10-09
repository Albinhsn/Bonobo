module Utils where

import Ast
import Lexer
import Token

parseIdentifier :: ([Token], [Statement]) -> ([Token], [Statement])
parseIdentifier (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == IDENT = parseAssign (removeFirstToken t, pop s ++ [Statement {statementType = LetStatement {identifier = literal (head t)}, expression = Expression {}}])
      | otherwise = error "Couldn't parse literal"

parseAssign :: ([Token], [Statement]) -> ([Token], [Statement])
parseAssign (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == ASSIGN = (removeFirstToken t, s)
      | otherwise = error "can't do let without =="

isArtithmetic :: Token -> Bool
isArtithmetic t = typ t == PLUS || typ t == ASTERISK || typ t == SLASH || typ t == MINUS

stringToInt :: String -> Int
stringToInt s = read s :: Int

removeFirstToken :: [Token] -> [Token]
removeFirstToken xs = case xs of
  [] -> []
  x : xs -> xs

isValidPrefix :: Token -> Bool
isValidPrefix t = typ t == MINUS || typ t == PLUS
