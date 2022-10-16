module Utils where

import Ast
import Data.Typeable
import Lexer
import Token

parseIdentifier :: ([Token], [Statement]) -> ([Token], [Statement])
parseIdentifier (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == IDENT = parseAssign (removeFirstToken t, pop s ++ [Statement {statementType = LetStatement {identifier = literal (head t)}, expression = Expression {expressionType = EMPTYEXP}}])
      | otherwise = error "Couldn't parse literal"

parseAssign :: ([Token], [Statement]) -> ([Token], [Statement])
parseAssign (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == ASSIGN = (removeFirstToken t, s)
      | otherwise = error "can't do let without =="

isPrefix :: Token -> Bool
isPrefix t = typ t == PLUS || typ t == ASTERISK || typ t == SLASH || typ t == MINUS || typ t == LESS_T || typ t == GREATER_T || typ t == EQUALS || typ t == NOT_EQUALS

stringToInt :: String -> Int
stringToInt s = read s :: Int

removeFirstToken :: [Token] -> [Token]
removeFirstToken xs = case xs of
  [] -> []
  x : xs -> xs

isValidInfix :: Token -> Bool
isValidInfix t = typ t == MINUS || typ t == BANG

isOperator :: Token -> Bool 
isOperator t = typ t == PLUS || typ t == ASTERISK || typ t == SLASH 

statementToString :: Statement -> String
statementToString s = str
  where
    str
      | statementType s == ReturnStatement = "return " ++ expressionToString (expression s) ++ ";"
      | statementType s == IfStatement = "doesn't exist yet" -- TODO FIX THIS
      | typeOf (statementType s) == typeRep LetStatement =
          "let "
            ++ identifier (statementType s)
            ++ " = "
            ++ expressionToString (expression s)
            ++ ";"
      | otherwise = error "error parsing statement to string "

expressionToString :: Expression -> String
expressionToString e = s
  where
    s
      | expressionType e == OPERATOREXP = expressionToString (leftOperator e) ++ " " ++ literal (operator e) ++ " " ++ expressionToString (rightOperator e)
      | expressionType e == INFIXEXP = literal (infixOperator e) ++ "" ++ expressionToString (infixExpression e)
      | expressionType e == INTEGERLITERALEXP = integerLiteral e
      | expressionType e == GROUPEDEXP = "doesn't exist yet" -- TODO fix this
      | expressionType e == PREFIXEXP = expressionToString (leftExpression e) ++ " " ++ literal (prefixOperator e) ++ " " ++ expressionToString (rightExpression e)
      | otherwise = error "couldn't parse type"

tokenToString :: Token -> String
tokenToString = literal
