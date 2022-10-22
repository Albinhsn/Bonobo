module Utils where

import Ast
import Data.Typeable
import Lexer
import Token




isBoolPrefix :: Token -> Bool
isBoolPrefix t = typ t == LESS_T || typ t == GREATER_T || typ t == EQUALS || typ t == NOT_EQUALS

stringToInt :: String -> Int
stringToInt s = read s :: Int

removeFirstToken :: [Token] -> [Token]
removeFirstToken xs = case xs of
  [] -> []
  x : xs -> xs

isValidInfix :: Token -> Bool
isValidInfix t = typ t == MINUS || typ t == BANG

isValidMinus:: Expression -> Bool
isValidMinus e = b
  where   
    b 
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) == EMPTYEXP = True 
      | expressionType e == OPERATOREXP = isValidMinus (rightOperator e) 
      | expressionType e == BOOLEXP && expressionType (rightBool e) == EMPTYEXP = True 
      | expressionType e == BOOLEXP = isValidMinus (rightBool e)
      | expressionType e == GROUPEDEXP = isValidMinus (groupedExpression e)
      | expressionType e == EMPTYEXP = True
      | otherwise = False 


isOperator :: Token -> Bool 
isOperator t = typ t == PLUS || typ t == ASTERISK || typ t == SLASH || typ t == MINUS 

statementToString :: Statement -> String
statementToString s = str
  where
    str
      | statementType s == RETSTA = "return " ++ expressionToString (expression s) ++ ";"
      | statementType s == IFSTA = "if " ++ expressionToString (expression s) ++ ifToString(s) ++ elseToString (s) 
      | statementType s == LETSTA=
          "let "
            ++ identifier (statementUni s)
            ++ " = "
            ++ expressionToString (expression s)
            ++ ";"
      | statementType s == ASSIGNSTA = expressionToString(assignIdent (expression s)) ++ " = " ++ expressionToString(assignExpression (expression s))  ++ ";"
      | otherwise = error "error parsing statement to string "

elseToString :: Statement -> String
elseToString s = str 
  where 
    str 
      | null (alt (statementUni s)) = "" 
      | otherwise = "{" ++(concat [statementToString x | x <- alt (statementUni s)]) ++  "}" 

ifToString :: Statement -> String
ifToString s = str 
  where 
    str 
      | null (con (statementUni s)) = "{}"
      | otherwise = "{" ++(concat [statementToString x | x <- con (statementUni s)]) ++  "}" 

expressionToString :: Expression -> String
expressionToString e = s
  where
    s
      | expressionType e == OPERATOREXP = "(" ++ expressionToString (leftOperator e) ++ " " ++ literal (operator e) ++ " " ++ expressionToString (rightOperator e) ++ ")"
      | expressionType e == INFIXEXP = "(" ++ literal (infixOperator e) ++ "" ++ expressionToString (infixExpression e) ++ ")"
      | expressionType e == INTEXP = integerLiteral e
      | expressionType e == GROUPEDEXP && closed e == False= "XX" ++ expressionToString (groupedExpression e) 
      | expressionType e == GROUPEDEXP = "(" ++ expressionToString (groupedExpression e) ++ ")" 
      | expressionType e == PREFIXEXP = expressionToString (leftExpression e) ++ " " ++ literal (prefixOperator e) ++ " " ++ expressionToString (rightExpression e)
      | expressionType e == BOOLEXP =  expressionToString (leftBool e) ++ " " ++ literal (boolOperator e) ++ " " ++ expressionToString (rightBool e)  
      | expressionType e == TFEXP && bool e == TRUE = "true"
      | expressionType e == TFEXP && bool e == FALSE = "false"
      | expressionType e == IDENTEXP = ident e 
      | expressionType e == EMPTYEXP = " empty "
      | otherwise = error "couldn't parse type"

tokenToString :: Token -> String
tokenToString t = literal t

getLastExpressionType:: (BlockType, [Statement]) -> ExpressionType  
getLastExpressionType (b, s) = e 
  where   
    e 
      | b == EXP || statementType (last s) /= IFSTA = expressionType (expression (last s)) 
      | b == CON && null (alt (statementUni (last s))) == True = getLastExpressionType (b, con(statementUni (last (s))))
      | b == CON = getLastExpressionType(b, alt(statementUni (last s)))
      | b == ALT && closedCon (statementUni (last s)) == True = getLastExpressionType (b, alt(statementUni (last (s))))
      | b == ALT = getLastExpressionType(b, con(statementUni (last s)))
      | b == CON = error "con"
      | b == ALT = error "alt"
      | otherwise = error "get last expressiontype"

getLastExpression:: [Statement] -> Expression
getLastExpression s = expression (last s)

getLastOperator :: [Statement] -> Token 
getLastOperator s = operator (expression (last s))

getLastLeftOperator :: [Statement] -> Expression 
getLastLeftOperator s = leftOperator (expression (last s))

getLastRightOperator :: [Statement] -> Expression 
getLastRightOperator s = rightOperator (expression (last s))

getLastInfixOperator :: [Statement] -> Token 
getLastInfixOperator s = infixOperator (expression (last s))

getLastLeftBool :: [Statement] -> Expression
getLastLeftBool s = leftBool (expression (last s))

getLastRightBool :: [Statement] -> Expression
getLastRightBool s = rightBool (expression (last s))

getLastBoolOperator :: [Statement] -> Token 
getLastBoolOperator s = boolOperator (expression (last s))

