module Compiler where 

import Code 
import Object 
import Ast
import Utils
import CompilerUtils
import Token
import Lexer
import Data.ByteString.UTF8 as BSU 
import Data.ByteString as BS 
import Data.Map as DM
import Debug.Trace
import Control.Lens

compile :: ([Statement], Compiler)-> Compiler 
compile (s,c) = comp 
  where 
    comp
      | Prelude.null s = 
        c 
      | statementType (Prelude.head s) == NOSTA = compile(removeFirst s, addToScope(compileExpression(
          expression (Prelude.head s),
          c),
        lookupOpCode OPPOP
        )
      )
      | statementType (Prelude.head s) == IFSTA= compile(removeFirst s, compileIf(CON, Prelude.head s, compileExpression(expression (Prelude.head s), c)))
      | statementType (Prelude.head s) == LETSTA = compile(removeFirst s, compileLet(Prelude.head s, c))
      | statementType (Prelude.head s) == ASSIGNSTA = compile(removeFirst s, compileAssign(expression (Prelude.head s),c))
      | statementType (Prelude.head s) == RETSTA = compile(removeFirst s, addToScope(compileExpression(expression (Prelude.head s), c), lookupOpCode OPRETURNVALUE ))
      | statementType (Prelude.head s) == FUNCSTA = compile(removeFirst s, compileFunc(Prelude.head s, c))
      | otherwise = error ("unkown statementType compiler " ++ (show (statementType (Prelude.head s)))) 

extractFunc :: (Int,Int,String,Compiler) -> Compiler
extractFunc (n,l,s,c) = addToScope(Compiler{
    scopes = pop (scopes c), 
    scopeIndex = scopeIndex c - 1,
    symbols = Symbol{symName = s, symIndex = Prelude.length (symbols c), symScope = getSymScope(scopeIndex c - 1)}:symbols c, 
    constants = constants c ++ [FuncObject{objectType = FUNC_OBJ, numArgs = n, numLocals = Prelude.length (constants c)-l, funcValue = scopes c!!(Prelude.length (scopes c) - 1)}]
  }, lookupOpCode OPCONST <> chooseToUnroll(Prelude.length (constants c)) <> lookupSetScope (scopeIndex c - 1) <> chooseToUnroll(Prelude.length (symbols c)))

compileFunc :: (Statement, Compiler) -> Compiler 
compileFunc (s, c) = comp 
  where 
    comp 
      | otherwise =  extractFunc(
        Prelude.length (params (statementUni s)),
        Prelude.length (constants c), 
        literal(ident (expression (s))), 
        compile(body (statementUni s), addSetParams(Prelude.length (params(statementUni s)), Compiler{
            scopes = scopes c,
            scopeIndex = scopeIndex c,
            constants = constants c,
            symbols = [
              Symbol{
                  symName = literal (ident x), 
                  symIndex =Prelude.length (symbols c) + i, 
                  symScope = LOCAL
                } | (x,i) <- Prelude.zip (params(statementUni s)) [0 ..] ] ++ symbols c
          })))

addSetParams :: (Int, Compiler) -> Compiler 
addSetParams (i, c) = generateSet(i, Compiler{
          scopes = scopes c++ [BS.empty :: ByteString],
          scopeIndex = scopeIndex c+ 1, 
          symbols =  symbols c, 
          constants = constants c
        })

generateSet :: (Int, Compiler) -> Compiler 
generateSet (i, c) = comp 
  where 
    comp 
      | i == 0 = c
      | otherwise = generateSet(i-1,addToScope(c, lookupOpCode SETLOCAL <> chooseToUnroll(Prelude.length (symbols c) - i)))

addToScope :: (Compiler, ByteString) -> Compiler 
addToScope (c, b) = 
  Compiler{
    constants = constants c, 
    symbols = symbols c,
    scopes = (scopes c) & element (scopeIndex c) .~ (scopes c!!scopeIndex c<> b), 
    scopeIndex = scopeIndex c 
  }

compileArrayIndex :: ([Expression],Compiler) -> Compiler 
compileArrayIndex (e, c) = comp
  where 
    comp
      | Prelude.null e = c 
      | otherwise = compileArrayIndex(pop e, compileExpression(Prelude.last e, c)) 


compileAssign :: (Expression, Compiler) -> Compiler 
compileAssign (e, c) = comp 
  where 
    comp 
      | expressionType e /= ASSIGNEXP = error "assignsta without assignexp?"
      | isSymbolName(symbols c, getAssignStrFromExp e)== False = error "can't assign to non existing variable" 
      | expressionType (assignIdent e) == INDEXEXP = addToScope(
          compileExpression(
            assignExpression e,
            compileArrayIndex(
              arrayIndex (assignIdent e), 
              c
            )
          ), 
          lookupGetScope (scopeIndex c) <> chooseToUnroll(getSymbolKey(symbols c, getAssignStrFromExp e)) <> lookupOpCode SETINDEX <> lookupSetScope (scopeIndex c)<>  chooseToUnroll(getSymbolKey(symbols c, getAssignStrFromExp e))
        )
      | otherwise = compileExpression(e, c) 

getAssignStrFromExp:: Expression -> String 
getAssignStrFromExp e = st 
  where 
    st 
      | expressionType e == ASSIGNEXP = getAssignStrFromExp(assignIdent e)
      | expressionType e == INDEXEXP = getAssignStrFromExp(arrayIdent e) 
      | expressionType e == IDENTEXP = literal (ident e) 
      | otherwise = error ("getAssignStrFromExp: " ++ (show e))


compileLet :: (Statement, Compiler) -> Compiler 
compileLet (s, c) = comp  
  where 
    comp
      | isSymbolName (symbols c, identifier (statementUni s))== True = error ("can't assign to already existing variable: " ++ identifier (statementUni s)) 
      | otherwise = 
        addToScope(compileExpression(expression s, Compiler{
          scopes = scopes c,
          scopeIndex = scopeIndex c,
          constants = constants c,
          symbols = Symbol{symName = identifier(statementUni s), symIndex = Prelude.length (symbols c), symScope = getSymScope (scopeIndex c)}:(symbols c)
        }), lookupSetScope (scopeIndex c) <> chooseToUnroll(Prelude.length (symbols c)))

lookupGetScope :: Int -> ByteString 
lookupGetScope i = 
  case i of 
    0 -> lookupOpCode GETGLOBAL
    _ -> lookupOpCode GETLOCAL

lookupSetScope :: Int -> ByteString 
lookupSetScope i = 
  case i of 
    0 -> lookupOpCode SETGLOBAL
    _ -> lookupOpCode SETLOCAL

getSymScope :: Int -> Scope 
getSymScope i = 
  case i of 
    0 -> GLOBAL
    _ -> LOCAL
      

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
            scopes = [BS.empty :: ByteString],
            scopeIndex = 0,
            constants = constants c,
            symbols = symbols c
          }
      ), c))
      | bl == ALT = addJump(compile(alt (statementUni s), Compiler{
          scopes = [BS.empty :: ByteString],
          scopeIndex = 0,
          constants = constants c,
          symbols = symbols c
        }), c)
      | otherwise = error "compileIf"

addJump :: (Compiler, Compiler) -> Compiler 
addJump (c, old) = addToScope(
    Compiler{
      symbols = symbols c,
      constants = constants c, 
      scopes = scopes old, 
      scopeIndex = scopeIndex old 
    },
    lookupOpCode JUMP <>chooseToUnroll (BS.length (scopes c!!scopeIndex c) + 2)  <>scopes c!!scopeIndex c
  )

addJumpNT :: (Compiler, Compiler) -> Compiler 
addJumpNT (c, old) =
  addToScope(
    Compiler{
      symbols = symbols c,
      constants = constants c, 
      scopes = scopes old, 
      scopeIndex = scopeIndex old 
    },
    lookupOpCode JUMPNT <>chooseToUnroll (BS.length (scopes c!!scopeIndex c) + 4)  <>scopes c!!scopeIndex c 
  )

compileExpression :: (Expression, Compiler) -> Compiler 
compileExpression (e,c) = comp 
  where 
    comp
      | expressionType e == OPERATOREXP = addOperatorInstruction(operator e, compileExpression(rightOperator e, compileExpression(leftOperator e, c)))
      | expressionType e == MAPEXP = addMapInstructions(
        mergeLists (fst (mapMap e)) (snd (mapMap e)), 
        addToScope(
          c,
          lookupOpCode HASHEND
        ))
      | expressionType e == ARRAYEXP = 
        addArrayInstructions(
          array e, 
          addToScope(
            c, 
            lookupOpCode ARRAYEND
          )
        )
      | expressionType e == INDEXEXP = addIndexInstructions(
          arrayIndex e, 
          compileExpression(arrayIdent e, c)
        ) 
      | expressionType e == INTEXP = 
        addToScope(
          Compiler{
              scopes = scopes c,
              scopeIndex = scopeIndex c,
              symbols = symbols c,
              constants = constants c ++ [IntObject{objectType = INT_OBJ, intValue = readIntFromString e}]
            },
          make(OPCONST, Prelude.length (constants c))
        ) 
      | expressionType e == STRINGEXP= addToScope(
        Compiler{
            scopes = scopes c,
            scopeIndex = scopeIndex c,
            constants = constants c ++ [StringObject{objectType = STRING_OBJ, stringValue = literal (stringLiteral e)}],
            symbols = symbols c
          },
          make(OPCONST, Prelude.length (constants c))
        ) 
      | expressionType e == GROUPEDEXP = compileExpression(groupedExpression e,c) 
      | expressionType e == BOOLEXP && typ (boolOperator e) /= LESS_T = addBoolInstruction(
          boolOperator e, 
          compileExpression(
            rightBool e, 
            compileExpression(leftBool e, c)
          )
        )
      | expressionType e == BOOLEXP = addBoolInstruction(
        Token{line = line (boolOperator e),typ = GREATER_T, literal = ">"}, 
        compileExpression(
          leftBool e, 
          compileExpression(rightBool e, c)
          )
        )
      | expressionType e == TFEXP = addToScope(c, lookupOpCode(compileTF e))
      | expressionType e == PREFIXEXP = addPrefixInstruction(
          prefixOperator e, 
          compileExpression(
            prefixExpression e, 
            c
            )
        ) 
      | expressionType e == IDENTEXP= addToScope(
        c,
        lookupGetScope (scopeIndex c) <> chooseToUnroll(getSymbolKey(symbols c, literal (ident e)))
      ) 
      | expressionType e == ASSIGNEXP = addAssignInstruction(
          getAssignStrFromExp (assignIdent e), 
          compileExpression(assignExpression e, c)
        ) 
      | expressionType e == CALLEXP = addCallInstructions(e, c)
      | otherwise = error (show e ++ " " ++ show (scopes c!!scopeIndex c) ++ " " ++ show (constants c) ++ " " ++ show (symbols c))

addCallInstructions ::(Expression, Compiler) -> Compiler 
addCallInstructions (e,c) = addToScope(c, lookupGetScope(scopeIndex c) <> chooseToUnroll(getSymbolKey(symbols c, literal (ident (callIdent e)))) <> lookupOpCode OPCALL <> chooseToUnroll(getSymbolKey(symbols c, literal (ident (callIdent e)))) <> addCallParams(callParams e, c)) 

getArgsFromSymbol :: (Compiler, String) -> Int 
getArgsFromSymbol (c, str) = i 
  where   
    i
      | objectType (constants c!! symIndex (Prelude.head [x | x <- symbols c, symName x== str])) /= FUNC_OBJ = error ("is not func: " ++ (show (constants c!! symIndex (Prelude.head [x | x <- symbols c, symName x== str]))))
      | otherwise = numArgs (constants c!! symIndex (Prelude.head [x | x <- symbols c, symName x== str]))

addCallParams :: ([Expression], Compiler) -> ByteString  
addCallParams (e, c) = b 
  where
    b
      | Prelude.null e = scopes c!!0 
      | otherwise = addCallParams(removeFirst e,compileExpression(Prelude.head e, c))


addAssignInstruction :: (String,Compiler) -> Compiler
addAssignInstruction (s, c) = addToScope(
    c,
    lookupSetScope (scopeIndex c)<> chooseToUnroll (getSymbolKey (symbols c, s))
  ) 

addIndexInstructions :: ([Expression], Compiler) -> Compiler 
addIndexInstructions (e, c) = comp
  where   
    comp
      | Prelude.null e = c 
      | otherwise = addIndexInstructions(
          removeFirst e, 
          addToScope(
            compileExpression(
              Prelude.head e,
              c 
            ), lookupOpCode INDEX
          )
        )

addMapInstructions :: ([Expression], Compiler) -> Compiler
addMapInstructions (e, c) = comp 
  where 
    comp 
      | Prelude.null e = addToScope(c, lookupOpCode HASH)
      | otherwise = addMapInstructions(pop e, compileExpression(Prelude.last e, c))


addArrayInstructions :: ([Expression], Compiler) -> Compiler
addArrayInstructions (e, c) = comp 
  where 
    comp 
      | Prelude.null e = addToScope(c, lookupOpCode ARRAY)
      | otherwise = addArrayInstructions(pop e, compileExpression(Prelude.last e, c))



addPrefixInstruction :: (Token, Compiler) -> Compiler 
addPrefixInstruction (t, c) = 
  case typ t of 
    MINUS -> addToScope(c, lookupOpCode OPMINUS)
    BANG -> addToScope(c, lookupOpCode OPBANG)
    _ -> error "non valid prefix"

compileTF :: Expression -> OpCode  
compileTF e =
  case bool e of 
    TRUE-> OPTRUE 
    FALSE -> OPFALSE
    _ -> error "how can this even throw an error"

addOperatorInstruction :: (Token, Compiler)-> Compiler 
addOperatorInstruction (t, c)=
  case typ t of 
    PLUS -> addToScope(c, lookupOpCode OPADD)
    SLASH -> addToScope(c, lookupOpCode OPDIV)
    ASTERISK -> addToScope(c, lookupOpCode OPMUL)
    MINUS -> addToScope(c, lookupOpCode OPSUB)
    _ -> error ("not an operator " ++ (literal t))

addBoolInstruction :: (Token, Compiler) -> Compiler 
addBoolInstruction (t, c) = 
  case typ t of 
    GREATER_T-> addToScope(c, lookupOpCode OPGT) 
    EQUALS -> addToScope(c, lookupOpCode OPEQ)
    NOT_EQUALS -> addToScope(c, lookupOpCode OPNEQ)
    _ -> error "cant add non valid bool instruction"

parseStatementToCompiled :: [Statement] -> Compiler 
parseStatementToCompiled s = compile(s, Compiler{
    scopes = [BS.empty :: ByteString],
    scopeIndex = 0,
    constants = [],
    symbols = []
  })
