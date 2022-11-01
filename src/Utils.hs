module Utils where

import Ast
import Lexer
import Token




isBoolPrefix :: Token -> Bool
isBoolPrefix t = typ t == LESS_T || typ t == GREATER_T || typ t == EQUALS || typ t == NOT_EQUALS

removeFirst:: [a] -> [a]
removeFirst xs = case xs of
  [] -> []
  x : xs -> xs

isValidPrefix:: Token -> Bool
isValidPrefix t = typ t == MINUS || typ t == BANG

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

statementsToString :: [Statement] -> String 
statementsToString s = deleteLast(concat [statementToString x ++ " " | x <- s])


statementToString :: Statement -> String
statementToString s = str
  where
    str
      | statementType s == RETSTA = "return " ++ expressionToString (expression s) ++ ";"
      | statementType s == IFSTA = "if" ++ expressionToString (expression s) ++ ifToString(closedCon (statementUni s), s) ++ elseToString (closedAlt (statementUni s), s) ++ ";" 
      | statementType s == LETSTA=
          "let "
            ++ identifier (statementUni s)
            ++ " = "
            ++ expressionToString (expression s)
            ++ ";"
      | statementType s == ASSIGNSTA = expressionToString(assignIdent (expression s)) ++ " = " ++ expressionToString(assignExpression (expression s))  ++ ";"
      | statementType s == NOSTA = expressionToString(expression (s))
      | statementType s == FUNCSTA = "fn" ++ " " ++ expressionToString(expression s) ++ paramToString(params (statementUni s)) ++ bodyToString(s) ++ ";"
      | statementType s == CALLSTA = expressionToString(expression s) ++ ";"
      | otherwise = error "error parsing statement to string "

elseToString :: (Bool, Statement) -> String
elseToString (b,s)= str 
  where 
    str 
      | null (alt (statementUni s)) = "" 
      | b == False = "else{" ++(concat [statementToString x | x <- alt(statementUni s)]) 
      | otherwise = "else{" ++(concat [statementToString x | x <- alt (statementUni s)]) ++  "}" 

paramToString :: [Expression ]-> String 
paramToString e = s 
  where   
    s 
      | null e = "()"
      | otherwise = "(" ++ deleteLast (concat [expressionToString x ++ "," | x <- e]) ++ ")"

callParamsToString :: Expression -> String 
callParamsToString e = s 
  where   
    s 
      | null (callParams e) = ""
      | otherwise = deleteLast (concat [expressionToString x ++ "," | x <- (callParams e)])

deleteLast :: [a] -> [a]
deleteLast [] = []
deleteLast [h] = []
deleteLast (h:t) = [h] ++ deleteLast t

bodyToString :: Statement -> String 
bodyToString s = str 
  where   
    str 
      | otherwise = "{" ++ pop (concat [statementToString x ++ " "| x <- body (statementUni s)]) ++ "}"


ifToString :: (Bool, Statement) -> String
ifToString (b, s) = str 
  where 
    str 
      | b == False= "{" ++(concat [statementToString x | x <- con (statementUni s)]) 
      | null (con (statementUni s)) = "{}"
      | otherwise = "{" ++(concat [statementToString x | x <- con (statementUni s)]) ++  "}" 

expressionToString :: Expression -> String
expressionToString e = s
  where
    s
      | expressionType e == OPERATOREXP = "(" ++ expressionToString (leftOperator e) ++ " " ++ literal (operator e) ++ " " ++ expressionToString (rightOperator e) ++ ")"
      | expressionType e == PREFIXEXP = "(" ++ literal (prefixOperator e) ++ "" ++ expressionToString (prefixExpression e) ++ ")"
      | expressionType e == INTEXP = literal (integerLiteral e)
      | expressionType e == GROUPEDEXP && closed e == False= "XX" ++ expressionToString (groupedExpression e) 
      | expressionType e == GROUPEDEXP = "(" ++ expressionToString (groupedExpression e) ++ ")" 
      | expressionType e == BOOLEXP =  expressionToString (leftBool e) ++ " " ++ literal (boolOperator e) ++ " " ++ expressionToString (rightBool e)  
      | expressionType e == TFEXP && bool e == TRUE = "True"
      | expressionType e == TFEXP && bool e == FALSE = "False"
      | expressionType e == IDENTEXP = literal (ident e)
      | expressionType e == EMPTYEXP = " empty "
      | expressionType e == CALLEXP = expressionToString(callIdent e) ++ "(" ++ callParamsToString(e) ++ ")"
      | expressionType e == ASSIGNEXP = expressionToString(assignIdent e) ++ " = " ++ expressionToString(assignExpression e) ++ ";"
      | expressionType e == STRINGEXP = "'" ++ literal (stringLiteral e) ++ "'" 
      | expressionType e == ARRAYEXP = "[" ++ (concat [expressionToString x ++ ", " | x <- array e]) ++ "]"
      | expressionType e == INDEXEXP = (expressionToString (arrayIdent e)) ++ "[" ++ (expressionToString(arrayIndex e)) ++ "]"
      | expressionType e == MAPEXP = "{" ++ concatMapMap(mapMap e) ++ "}"
      | otherwise = error "couldn't parse type"


concatMapMap :: ([Expression], [Expression]) -> String 
concatMapMap (key, val) = s 
  where
    s 
      | null key && null val = ""
      | null val = expressionToString (head key)
      | otherwise = concat [expressionToString i ++ ":" ++ expressionToString x ++ ", " | (i, x) <- zip key val] 

tokenToString :: Token -> String
tokenToString t = s 
  where
    s
      | typ t == STRING = "'" ++ literal t ++ "'"
      | otherwise = literal t

getLastExpressionType:: (BlockType, [Statement]) -> ExpressionType  
getLastExpressionType (b, s) = e 
  where   
    e 
      | statementType (last s) == FUNCSTA && null (params (statementUni (last s))) = EMPTYEXP
      | statementType (last s) == FUNCSTA = expressionType (last(params(statementUni(last s))))
      | b == EXP || statementType (last s) /= IFSTA = expressionType (expression (last s)) 
      | b == PAR = error "getLastExpressionType not implemented for PAR"
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

getLastPrefixOperator :: [Statement] -> Token 
getLastPrefixOperator s = prefixOperator (expression (last s))

getLastLeftBool :: [Statement] -> Expression
getLastLeftBool s = leftBool (expression (last s))

getLastRightBool :: [Statement] -> Expression
getLastRightBool s = rightBool (expression (last s))

getLastBoolOperator :: [Statement] -> Token 
getLastBoolOperator s = boolOperator (expression (last s))

getTokens :: (Int, String, [Token]) -> [Token]
getTokens (i,s,t) = t
