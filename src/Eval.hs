module Eval where 

import Object
import Parser
import Ast 
import Lexer 
import Token 
import Utils 


evaluateExpression :: Expression -> Object  
evaluateExpression e = o 
  where 
    o
      | expressionType e == INTEXP = IntObject{objectType = INT_OBJ, intValue = parseIntFromToken(integerLiteral e)}
      | expressionType e == TFEXP = BoolObject{objectType = BOOL_OBJ, boolValue = parseBoolFromTokenType(bool e)}
      | expressionType e == PREFIXEXP = PrefixObject{objectType = PREFIX_OBJ, prefixValue = parsePrefixValue(e)}
      | otherwise = error "error evaluating expression"
