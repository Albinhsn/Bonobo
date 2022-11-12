module Compiler where 
import Debug.Trace

import Code 
import Object 
import Ast
import Utils
import Token
import Lexer
import Data.Word (Word8)
import Data.ByteString.UTF8 as BSU 
import Data.ByteString as BS 
import Data.Map as DM
import Numeric (showHex)





compile :: ([Statement], Compiler)-> Compiler 
compile (s,c) = comp 
  where 
    comp
      | Prelude.null s = c 
      | statementType (Prelude.head s) == NOSTA = compile(removeFirst s, compileExpression(
          expression (Prelude.head s),
          c)) 
      | statementType (Prelude.head s) == NOSTA = compile(removeFirst s, compileExpression(expression (Prelude.head s), c)) 
      | statementType (Prelude.head s) == IFSTA= compileIf(CON, Prelude.head s, compileExpression(expression (Prelude.head s), c))
      | statementType (Prelude.head s) == LETSTA = compile(removeFirst s, compileLet(Prelude.head s, c))
      | otherwise = error ("unkown statementType compiler " ++ (show (statementType (Prelude.head s)))) 

addLet :: Compiler -> Compiler 
addLet c =Compiler{
    symbols = symbols c,
    bytes = bytes c <> lookupOpCode(SETGLOBAL) <> chooseToUnroll(Prelude.length (symbols c) - 1),
    constants = constants c
  } 

compileLet :: (Statement, Compiler) -> Compiler 
compileLet (s, c) = comp  
  where 
    comp
      | member (identifier (statementUni s)) (fromList (symbols c)) == True = error ("can't assign to already existing variable: " ++ identifier (statementUni s)) 
      | otherwise = addLet(compileExpression(expression s, Compiler{
          bytes = bytes c,
          constants = constants c,
          symbols = (identifier (statementUni s), Prelude.length (symbols c)):(symbols c)
        }))

compileIf :: (BlockType, Statement, Compiler) -> Compiler 
compileIf (bl, s, c) = comp 
  where 
    comp
      | bl == CON = compileIf(ALT, Statement{
          closedSta = False,
          staLine = staLine s,
          statementType = IFSTA, 
          statementUni = IfStatement{
              closedCon = True,
              con = [],
              alt = alt (statementUni s),
              closedAlt = True
            },
          expression = expression s
        }, 
        addJumpNT(compile(con (statementUni s), Compiler{
            bytes = BS.empty :: ByteString,
            constants = constants c,
            symbols = symbols c
          }
      ), bytes c))
      | bl == ALT = addJump(compile(alt (statementUni s), Compiler{
          bytes = BS.empty :: ByteString,
          constants = constants c,
          symbols = symbols c
        }), bytes c)
      | otherwise = error "compileIf"


addJump :: (Compiler, ByteString) -> Compiler 
addJump (c, b) = Compiler{
    symbols = symbols c,
    constants = constants c, 
    bytes = b <> lookupOpCode JUMP <> chooseToUnroll (BS.length (bytes c) + 2) <> bytes c
  }

addJumpNT :: (Compiler, ByteString) -> Compiler 
addJumpNT (c, b) = Compiler{
    symbols = symbols c, 
    constants = constants c, 
    bytes = b <> lookupOpCode JUMPNT <> chooseToUnroll (BS.length (bytes c) + 4) <> bytes c
  } 

compileExpression :: (Expression, Compiler) -> Compiler 
compileExpression (e,c) = comp 
  where 
    comp
      | expressionType e == OPERATOREXP = addOperatorInstruction(operator e, compileExpression(rightOperator e, compileExpression(leftOperator e, c)))
      | expressionType e == ARRAYEXP = addArrayInstructions(array e, c)  
      | expressionType e == INTEXP = Compiler{
          bytes = bytes c <> make(OPCONST, Prelude.length (constants c)),
          constants = constants c ++ [IntObject{objectType = INT_OBJ, intValue = readIntFromString e}],
          symbols = symbols c
        }
      | expressionType e == STRINGEXP= Compiler{
          bytes = bytes c <> make(OPCONST, Prelude.length (constants c)),
          constants = constants c ++ [StringObject{objectType = STRING_OBJ, stringValue = literal (stringLiteral e)}],
          symbols = symbols c
        }
      | expressionType e == GROUPEDEXP = compileExpression(groupedExpression e,c) 
      | expressionType e == BOOLEXP && typ (boolOperator e) /= LESS_T = addBoolInstruction(boolOperator e, compileExpression(rightBool e, compileExpression(leftBool e, c)))
      | expressionType e == BOOLEXP = addBoolInstruction(Token{line = line (boolOperator e),typ = GREATER_T, literal = ">"}, compileExpression(leftBool e, compileExpression(rightBool e, c)))
      | expressionType e == TFEXP = Compiler{
          bytes = bytes c <> lookupOpCode(compileTF e),
          constants = constants c,
          symbols = symbols c
        }
      | expressionType e == PREFIXEXP = addPrefixInstruction(prefixOperator e, compileExpression(prefixExpression e, c)) 
      | expressionType e == IDENTEXP= Compiler{
          bytes = bytes c <> lookupOpCode GETGLOBAL <> chooseToUnroll(getSymbolKey(literal (ident e), symbols c)),
          constants = constants c,
          symbols = symbols c
        }
      | otherwise = error (show e ++ " " ++ show (bytes c) ++ " " ++ show (constants c) ++ " " ++ show (symbols c))

addArrayInstructions :: ([Expression], Compiler) -> Compiler
addArrayInstructions (e, c) = comp 
  where 
    comp 
      | Prelude.null e = Compiler{
          bytes = bytes c <> lookupOpCode ARRAY, 
          constants = constants c,
          symbols = symbols c 
        }
      | otherwise = addArrayInstructions(pop e, compileExpression(Prelude.last e, c))


getSymbolKey :: (String, [(String, Int)]) -> Int  
getSymbolKey (s, sym)= i 
  where 
    i 
      | member s (fromList sym) == False = error ("Trying to access variable that doesn't exist: " ++ s)
      | otherwise = (fromList sym) ! s

addPrefixInstruction :: (Token, Compiler) -> Compiler 
addPrefixInstruction (t, c) = 
  case typ t of 
    MINUS -> Compiler{
        bytes = bytes c <> lookupOpCode OPMINUS,
        constants = constants c,
        symbols = symbols c
      }
    BANG -> Compiler{
        bytes = bytes c <> lookupOpCode OPBANG,
        constants = constants c,
        symbols = symbols c
      }

compileTF :: Expression -> OpCode  
compileTF e =
  case bool e of 
    TRUE-> OPTRUE 
    FALSE -> OPFALSE

addPopInstruction :: Compiler -> Compiler  
addPopInstruction c = Compiler{
    bytes = bytes c <> lookupOpCode(OPPOP),
    constants = constants c,
    symbols = symbols c
  }

addOperatorInstruction :: (Token, Compiler)-> Compiler 
addOperatorInstruction (t, c)=
  case typ t of 
    PLUS -> Compiler{
        bytes = bytes c <> lookupOpCode OPADD, 
        constants = constants c,
        symbols = symbols c
      }
    SLASH -> Compiler{
        bytes = bytes c <> lookupOpCode OPDIV, 
        constants = constants c,
        symbols = symbols c
      }
    ASTERISK -> Compiler{
        bytes = bytes c <> lookupOpCode OPMUL, 
        constants = constants c,
        symbols = symbols c
      }
    MINUS -> Compiler{
        bytes = bytes c <> lookupOpCode OPSUB, 
        constants = constants c,
        symbols = symbols c
      }
    _ -> error ("not an operator " ++ (literal t))

addBoolInstruction :: (Token, Compiler) -> Compiler 
addBoolInstruction (t, c) = 
  case typ t of 
    GREATER_T-> Compiler{
        bytes = bytes c <> lookupOpCode OPGT, 
        constants = constants c,
        symbols = symbols c
      }
    EQUALS -> Compiler{
        bytes = bytes c <> lookupOpCode OPEQ, 
        constants = constants c,
        symbols = symbols c
      }
    NOT_EQUALS -> Compiler{
        bytes = bytes c <> lookupOpCode OPNEQ, 
        constants = constants c,
        symbols = symbols c
      }
