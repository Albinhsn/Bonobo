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
      | statementType (Prelude.head s) == NOSTA = compile(removeFirst s, compileExpression(expression (Prelude.head s),(b,o)))
      | statementType (Prelude.head s) == IFSTA = 
          --Go to next statement in block
          compile(
            removeFirst s, 
            compile( --Parse alt
              alt(statementUni (Prelude.last s)),
              addJump(compile(con (statementUni(Prelude.last s)),addJumpNT(compileExpression(expression (Prelude.head s), (b, o))))))) --Parse con 
      | otherwise = error "compile" 

addJump :: (ByteString, [Object]) -> (ByteString, [Object])
addJump (b, o) = ( b<> lookupOpCode JUMP,o )

addJumpNT :: (ByteString, [Object]) -> (ByteString, [Object])
addJumpNT (b, o) = ( b<> lookupOpCode JUMPNT,o )

compileExpression :: (Expression, (ByteString, [Object])) -> (ByteString, [Object]) 
compileExpression (e,(b, o)) = (by, ob) 
  where 
    (by, ob)
      | expressionType e == OPERATOREXP = addOperatorInstruction(operator e, compileExpression(rightOperator e, compileExpression(leftOperator e, (b, o))))
      | expressionType e == INTEXP = (b <> make(OPCONST, Prelude.length o), o ++ [IntObject{objectType = INT_OBJ, intValue = readIntFromString e}]) 
      | expressionType e == GROUPEDEXP = compileExpression(groupedExpression e,(b,o)) 
      | expressionType e == BOOLEXP && typ (boolOperator e) /= LESS_T = addBoolInstruction(boolOperator e, compileExpression(rightBool e, compileExpression(leftBool e, (b, o))))
      | expressionType e == BOOLEXP = addBoolInstruction(Token{line = line (boolOperator e),typ = GREATER_T, literal = ">"}, compileExpression(leftBool e, compileExpression(rightBool e, (b, o))))
      | expressionType e == TFEXP = (b <> lookupOpCode(compileTF e), o)
      | expressionType e == PREFIXEXP = addPrefixInstruction(prefixOperator e, compileExpression(prefixExpression e, (b,o))) 
      | otherwise = error (show e ++ " " ++ show b ++ " " ++ show o)


addPrefixInstruction :: (Token, (ByteString, [Object])) -> (ByteString, [Object])
addPrefixInstruction (t, (b, o)) = 
  case typ t of 
    MINUS -> (b <> lookupOpCode (OPMINUS), o)
    BANG -> (b <> lookupOpCode (OPBANG), o)

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

addBoolInstruction :: (Token, (ByteString, [Object])) -> (ByteString, [Object])
addBoolInstruction (t, (b, o)) = 
  case typ t of 
    GREATER_T -> (b <> lookupOpCode (OPGT), o) 
    EQUALS -> (b <> lookupOpCode (OPEQ), o) 
    NOT_EQUALS -> (b <> lookupOpCode (OPNEQ), o) 
