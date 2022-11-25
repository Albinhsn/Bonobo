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
      | statementType (Prelude.head s) == CALLSTA = compile(removeFirst s, compileExpression(expression (Prelude.head s), c))
      | otherwise = error ("unkown statementType compiler " ++ (show (statementType (Prelude.head s)))) 

extractFunc :: (Int,String,Compiler) -> Compiler
extractFunc (n,s,c) = 
  addToLastSymbol(
  Symbol{symName = s, symIndex = Prelude.length (symbols c!!(Prelude.length (symbols c)-2)), symScope = getSymScope(scopeIndex c - 1)},
  addToScope(Compiler{
    scopes = pop (scopes c), 
    scopeIndex = scopeIndex c - 1,
    symbols =  pop(symbols c), 
    constants = constants c ++ [FuncObject{objectType = FUNC_OBJ, numArgs = n, numLocals = Prelude.length (symbols c!!(Prelude.length (symbols c) - 1 )), funcValue = scopes c!!(Prelude.length (scopes c) - 1) <> lookupOpCode OPRETURN}]
  }, lookupOpCode OPCONST <> chooseToUnroll(Prelude.length (constants c)) <> lookupSetScope (scopeIndex c - 1 , s, c) <> chooseToUnroll(Prelude.length (symbols c !!(Prelude.length (symbols c) - 2)))))

addToLastSymbol :: (Symbol, Compiler) -> Compiler 
addToLastSymbol (s, c) = 
  Compiler{
    scopes = scopes c, 
    scopeIndex = scopeIndex c,
    constants = constants c,
    symbols = pop (symbols c) ++ [(s:Prelude.last (symbols c))]
  }

compileFunc :: (Statement, Compiler) -> Compiler 
compileFunc (s, c) = comp 
  where 
    comp 
      | otherwise =  
        extractFunc(
          Prelude.length (params (statementUni s)),
          literal(ident (expression (s))), 
          compile(body (statementUni s), enterScope(Compiler{
              scopes = scopes c,
              scopeIndex = scopeIndex c,
              constants = constants c,
              symbols = symbols c ++ [[
                Symbol{
                    symName = literal (ident x), 
                    symIndex =i, 
                    symScope = LOCAL
                  } | (x,i) <- Prelude.zip (params(statementUni s)) [0 ..] ]]
          })))

enterScope :: Compiler -> Compiler 
enterScope c= Compiler{
          scopes = scopes c++ [BS.empty :: ByteString],
          scopeIndex = scopeIndex c+ 1, 
          symbols =  symbols c, 
          constants = constants c
        }

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
      | isSymbolName(Prelude.length (symbols c) -1, symbols c, getAssignStrFromExp e)== False = error "can't assign to non existing variable" 
      | expressionType (assignIdent e) == INDEXEXP = addToScope(
          compileExpression(
            assignExpression e,
            compileArrayIndex(
              arrayIndex (assignIdent e), 
              c
            )
          ), 
          getScopeAndKey(getAssignStrFromExp e, c) <> lookupOpCode SETINDEX <> lookupSetScope (scopeIndex c, getAssignStrFromExp e, c)<>  chooseToUnroll(getSymbolKey(symbols c, getAssignStrFromExp e))
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
      | isSymbolName (Prelude.length (symbols c) - 1, symbols c, identifier (statementUni s))== True = error ("can't assign to already existing variable: " ++ identifier (statementUni s)) 
      | otherwise = 
        addToLastSymbol(Symbol{symName = identifier(statementUni s), symIndex = Prelude.length (symbols c!!(Prelude.length (symbols c) - 1)), symScope = getSymScope (scopeIndex c)},addToScope(compileExpression(expression s, Compiler{
          scopes = scopes c,
          scopeIndex = scopeIndex c,
          constants = constants c,
          symbols = symbols c 
        }), lookupLetScope (scopeIndex c) <> chooseToUnroll(Prelude.length (symbols c!!(Prelude.length (symbols c ) - 1)))))

getScopeAndKey :: (String, Compiler) -> ByteString 
getScopeAndKey (s, c) = b 
  where   
    b 
      | scopeIndex c == 0 =lookupOpCode GETGLOBAL <> chooseToUnroll(symIndex (Prelude.head [x | x <- symbols c !! 0, symName x == s])) 
      | Prelude.null [x | x <- symbols c !! 0, symName x == s] = lookupOpCode GETLOCAL <>  chooseToUnroll(getSymbolKey(symbols c, s)) 
      | otherwise = lookupOpCode GETGLOBAL <> chooseToUnroll(getSymbolKey(symbols c, s)) 

lookupSetScope :: (Int, String,Compiler) -> ByteString 
lookupSetScope (i, s, c)= b 
  where 
    b 
      | i == 0 = lookupOpCode SETGLOBAL 
      | Prelude.null [x | x <- symbols c!!0, symName x == s] == True = lookupOpCode SETLOCAL 
      | otherwise = lookupOpCode SETGLOBAL

lookupLetScope :: Int -> ByteString 
lookupLetScope i = 
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
        Prelude.length (fst (mapMap e)), 
        mergeLists (fst (mapMap e)) (snd (mapMap e)),
        c
        )
      | expressionType e == ARRAYEXP = 
        addArrayInstructions(
          Prelude.length (array e),
          array e, 
          c
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
        -- lookupGetScope (scopeIndex c) <> chooseToUnroll(getSymbolKey(symbols c, literal (ident e)))
        getScopeAndKey(literal (ident e), c) 
      ) 
      | expressionType e == ASSIGNEXP = addAssignInstruction(
          getAssignStrFromExp (assignIdent e), 
          compileExpression(assignExpression e, c)
        ) 
      | expressionType e == CALLEXP = addCallInstructions(e, c)
      | otherwise = error (show e ++ " " ++ show (scopes c!!scopeIndex c) ++ " " ++ show (constants c) ++ " " ++ show (symbols c))

addCallInstructions ::(Expression, Compiler) -> Compiler 
addCallInstructions (e,c) = addToScope(addCallParams((callParams e), c), getScopeAndKey(literal (ident (callIdent e)), c)<> lookupOpCode OPCALL <> chooseToUnroll(getSymbolKey(symbols c, literal (ident (callIdent e)))))
-- addCallInstructions (e,c) = addToScope(addCallParams(Prelude.reverse (callParams e), c), lookupGetScope(scopeIndex c) <> chooseToUnroll(getSymbolKey(symbols c, literal (ident (callIdent e)))) <> lookupOpCode OPCALL <> chooseToUnroll(getSymbolKey(symbols c, literal (ident (callIdent e)))))

getArgsFromSymbol :: (Compiler, String) -> Int 
getArgsFromSymbol (c, str) = i 
  where   
    i
      | objectType (constants c !!symIndex (findSymbol (c, str))) /= FUNC_OBJ = error ("is not func: " ++ str)
      | otherwise = numArgs (constants c!!symIndex(findSymbol (c, str)))

findSymbol :: (Compiler, String) -> Symbol
findSymbol (c, s) = sym 
  where 
    sym
      | Prelude.length (symbols c) == 1 && Prelude.null [x | x <- symbols c!!0, symName x == s] == True = error ("Couldn't find symbol: " ++ s)
      | Prelude.null [x | x <- symbols c!!(Prelude.length (symbols c) - 1), symName x == s] == True = error ("Couldn't find symbol: " ++ s)
      | otherwise = Prelude.head [x | x <- (symbols c!!(Prelude.length (symbols c) - 1)), symName x == s]

addCallParams :: ([Expression], Compiler) -> Compiler 
addCallParams (e, c) = comp 
  where
    comp  
      | Prelude.null e = c 
      | otherwise = addCallParams(removeFirst e,compileExpression(Prelude.head e, c))


addAssignInstruction :: (String,Compiler) -> Compiler
addAssignInstruction (s, c) = addToScope(
    c,
    lookupSetScope (scopeIndex c, s, c)<> chooseToUnroll (getSymbolKey (symbols c, s))
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

addMapInstructions :: (Int, [Expression], Compiler) -> Compiler
addMapInstructions (i, e, c) = comp 
  where 
    comp 
      | Prelude.null e = addToScope(c, lookupOpCode HASH <> chooseToUnroll (i * 2))
      | otherwise = addMapInstructions(i, removeFirst e, compileExpression(Prelude.head e, c))


addArrayInstructions :: (Int, [Expression], Compiler) -> Compiler
addArrayInstructions (i, e, c) = comp 
  where 
    comp 
      | Prelude.null e = addToScope(c, lookupOpCode ARRAY <> chooseToUnroll i)
      | otherwise = addArrayInstructions(i, removeFirst e, compileExpression(Prelude.head e, c))



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
    symbols = [[]]
  })
