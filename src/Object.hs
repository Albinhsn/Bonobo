module Object where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 


data ObjectType = NULL_OBJ | INT_OBJ | BOOL_OBJ 


data Object 
  = NullObject {objectType :: !ObjectType}
  | IntObject {objectType :: !ObjectType, intValue :: !Int}
  | BoolObject {objectType :: !ObjectType, boolValue :: !Bool}


inspectObject :: Object -> String 
inspectObject o = 
  case objectType o of 
    NULL_OBJ -> "null"
    INT_OBJ -> show (intValue o)
    BOOL_OBJ -> show(boolValue o) 

parseIntFromToken :: Token -> Int 
parseIntFromToken t = read (literal t)

parseBoolFromTokenType :: TokenType -> Bool 
parseBoolFromTokenType t = 
  case t of 
    TRUE -> True 
    FALSE -> False 
    _ -> error "parseBoolFromTokenType"

parsePrefixValue :: Expression -> Bool
parsePrefixValue e =  
  case typ (prefixOperator e) of 
    BANG ->  parseBangValue(prefixExpression e) 
    MINUS -> parseMinusValue(prefixExpression e)

parseBangValue :: Expression -> Bool 
parseBangValue e = b 
  where 
    b 
      | otherwise = error "not implemented"

parseMinusValue :: Expression -> Bool 
parseMinusValue e = b 
  where 
    b 
      | otherwise = error "not implemented"
