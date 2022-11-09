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
      | expressionType e == INTEXP = (b <> make(OPCONST, Prelude.length o), o ++ [IntObject{objectType = INT_OBJ, intValue = readIntFromString e}]) 
      | expressionType e == GROUPEDEXP = compileExpression(groupedExpression e,(b,o)) 
      | expressionType e == BOOLEXP && typ (boolOperator e) /= LESS_T = addBoolInstruction(boolOperator e, compileExpression(rightBool e, compileExpression(leftBool e, (b, o))))
      | expressionType e == BOOLEXP = addBoolInstruction(boolOperator e, compileExpression(leftBool e, compileExpression(rightBool e, (b, o))))
      | expressionType e == TFEXP = (b <> lookupOpCode(compileTF e), o)
      | otherwise = error (show e ++ " " ++ show b ++ " " ++ show o)

compileTF :: Expression -> OpCode  
compileTF e =
  case bool e of 
    TRUE-> OPTRUE 
    FALSE -> OPFALSE

addPopInstruction :: (ByteString, [Object]) -> (ByteString, [Object])
addPopInstruction (b, o) = (b <> lookupOpCode(OPPOP), o)

addOperatorInstruction :: (Token, (ByteString, [Object])) -> (ByteString, [Object]) 
addOperatorInstruction (t, (b, o)) =
  case typ t of 
    PLUS -> (b <> lookupOpCode (OPADD),o)
    SLASH -> (b <> lookupOpCode (OPDIV), o)
    ASTERISK -> (b <> lookupOpCode (OPMUL), o)
    MINUS -> (b <> lookupOpCode(OPSUB), o)
    _ -> error ("not an operator " ++ (literal t))
addBoolInstruction :: (Token, (Bytestring, [Object])) -> (ByteString, [Object])
addBoolInstruction (t, (b, o)) = 
  case typ t of 
    GREATER_T -> (b <> lookupOpCode (OPGT)) 
