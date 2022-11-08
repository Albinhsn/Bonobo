module Compiler where 
import Debug.Trace

import Code 
import Object 
import Ast
import Utils
import Token

import Data.Word (Word8)
import Data.ByteString.UTF8 as BSU 
import Data.ByteString as BS 
import Data.Map as DM
import Numeric (showHex)





compile :: ([Statement],(ByteString,[Object])) ->(ByteString, [Object]) 
compile (s, (b, o)) = (by, ob) 
  where 
    (by, ob) 
      | Prelude.null s = (b, o)
      | statementType (Prelude.head s) == NOSTA = (compile(removeFirst s, compileExpression(expression (Prelude.head s),(b,o))))
      | otherwise = error "compile" 



compileExpression :: (Expression, (ByteString, [Object])) -> (ByteString, [Object]) 
compileExpression (e,(b, o)) = (by, ob) 
  where 
    (by, ob)
      | expressionType e == OPERATOREXP = addOperatorInstruction(operator e, compileExpression(rightOperator e, compileExpression(leftOperator e, (b, o))))
      | expressionType e == INTEXP = (b <> make(CONST, Prelude.length o), o ++ [IntObject{objectType = INT_OBJ, intValue = readIntFromString e}]) 
      | otherwise = error (show e ++ " " ++ show b ++ " " ++ show o)


addPopInstruction :: (ByteString, [Object]) -> (ByteString, [Object])
addPopInstruction (b, o) = (b <> lookupOpCode(POP), o)

addOperatorInstruction :: (Token, (ByteString, [Object])) -> (ByteString, [Object]) 
addOperatorInstruction (t, (b, o)) =
  case typ t of 
    PLUS -> (b <> lookupOpCode (ADD),o)
    SLASH -> (b <> lookupOpCode (DIV), o)
    ASTERISK -> (b <> lookupOpCode (MUL), o)
    MINUS -> (b <> lookupOpCode(SUB), o)
    _ -> error ("not an operator " ++ (literal t))
