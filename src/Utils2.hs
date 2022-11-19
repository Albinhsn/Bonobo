module Utils2 (
  reverseList,
  parseStringToStatementsString,
  parseStringToStatements,
  parseMakeToPretty,
  parseStatementToCompiled,
  inspectGlobal,
  inspectObject,
  disassemble

) where

import Debug.Trace
import Object 
import Ast
import Compiler
import Code 
import Lexer 
import Utils
import Parser
import VM


import Control.Lens
import Data.ByteString as BS

reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = Prelude.last xs : reverseList (Prelude.init xs)

parseStringToStatementsString :: String -> String 
parseStringToStatementsString s =  statementsToString(snd(snd( parseStatements(EXP,( getTokens(parseTokens(0, s, [])),[])))))

parseStringToStatements :: String -> [Statement] 
parseStringToStatements s = snd(snd( parseStatements(EXP,( getTokens(parseTokens(0, s, [])),[]))))


parseMakeToPretty :: (ByteString, [Object]) -> String 
parseMakeToPretty (b, o) = (prettyPrint b ++ " - " ++ Prelude.concat [inspectObject x ++ " " | x  <- o])

parseStatementToCompiled :: [Statement] -> Compiler 
parseStatementToCompiled s = compile(s, Compiler{
    scopes = [BS.empty :: ByteString],
    scopeIndex = 0,
    constants = [],
    symbols = []
  })


disassemble :: (String, Compiler) -> String 
disassemble (s,c)= str 
  where 
    str 
      | BS.null (scopes c!!scopeIndex c) = s
      | BS.head (scopes c!!scopeIndex c) == 0 = disassemble(s ++ " CONST " ++  (show (BS.index (scopes c!!scopeIndex c) 1)), removeFromScope(removeFromScope c))
      --Pop
      | BS.head (scopes c!!scopeIndex c) == 1 = disassemble(s ++ " POP", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 2 = disassemble(s ++ " ADD", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 3 = disassemble(s ++ " SUB", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 4 = disassemble(s ++ " MUL", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 5 = disassemble(s ++ " DIV", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 6 = disassemble(s ++ " TRUE", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 7 = disassemble(s ++ " FALSE", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 8 = disassemble(s ++ " GT", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 9 = disassemble(s ++ " LT", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 10 = disassemble(s ++ " NEQ", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 11 = disassemble(s ++ " EQ", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 12 = disassemble(s ++ " MINUS", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 13 = disassemble(s ++ " BANG", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 14 = disassemble(s ++ " JUMP " ++ (show (BS.index (scopes c!!scopeIndex c) 1)),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 15 = disassemble(s ++ " JUMPNT " ++ (show (BS.index (scopes c!!scopeIndex c) 1)),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 16 = 
        disassemble(s ++ " SETGLOBAL " ++ (findSymbolFromVal(symbols c,(fromInteger(fromIntegral(BS.index (scopes c!!scopeIndex c) 1))))),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 17 = 
        disassemble(s ++ " GETGLOBAL " ++ (findSymbolFromVal(symbols c, (fromInteger(fromIntegral(BS.index (scopes c!!scopeIndex c) 1))))),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 18 = disassemble(s ++ " ARRAY", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 19 = disassemble(s ++ " ARRAYEND", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 20 = disassemble(s ++ " HASH", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 21 = disassemble(s ++ " HASHEND", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 22 = disassemble(s ++ " INDEX", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 23 = disassemble(s ++ " SETINDEX", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 24 = disassemble(s ++ " OPCALL", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 25 = disassemble(s ++ " RETURNVALUE ", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 26 = disassemble(s ++ " OPRETURN ", removeFromScope c)
      | otherwise = error ("disassemble " ++ (show (BS.head (scopes c!!scopeIndex c))))

removeFromScope :: Compiler -> Compiler 
removeFromScope c = Compiler{
    constants = constants c,
    symbols = symbols c, 
    scopes = (scopes c) & element (scopeIndex c) .~ (removeFirstInstruction(scopes c!!scopeIndex c)),
    scopeIndex = scopeIndex c 
  }

findSymbolFromVal :: ([(String, Int)], Int) -> String 
findSymbolFromVal (symbols, val) = 
  trace (show symbols ++ " " ++ show val)
  Prelude.head [x | (x,i) <- symbols, val == i]

-- removeFirstInstruction :: ByteString -> ByteString 
-- removeFirstInstruction b = 
--   case BS.length b of 
--     0 -> error "can't remove instruction of length 0?"
--     1 -> BS.empty :: ByteString 
--     _ -> pack(removeFirst(unpack b))

