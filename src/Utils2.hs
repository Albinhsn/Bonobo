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


import Object 
import Ast
import Compiler
import Code 
import Lexer 
import Utils
import Parser
import VM


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
    scopes = [],
    scopeIndex = 0,
    constants = [],
    symbols = []
  })


-- parseStack ::  VM -> String 
-- parseStack v = ("Stack: " ++ Prelude.concat [inspectObject x | x <- (stack v)] ++ " " ++ "Globals: " ++ Prelude.concat [inspectGlobal x | x <- (global v)])

inspectGlobal :: (Int, Object) -> String 
inspectGlobal (i, o) = (show i) ++ " = " ++ inspectObject o ++ " "


disassemble :: (String, ByteString) -> String 
disassemble (s,bytes)= str 
  where 
    str 
      | BS.null bytes = s
      | BS.head bytes == 0 = disassemble(s ++ " CONST " ++  (show (index (bytes) 1)), 
          removeFirstInstruction(removeFirstInstruction bytes))
      --Pop
      | BS.head bytes == 1 = disassemble(s ++ " POP",
          removeFirstInstruction bytes
        )
      | BS.head bytes == 2 = disassemble(s ++ " ADD",
          removeFirstInstruction bytes)
      | BS.head bytes == 3 = disassemble(s ++ " SUB",
          removeFirstInstruction bytes)
      | BS.head bytes == 4 = disassemble(s ++ " MUL",
          removeFirstInstruction bytes)
      | BS.head bytes == 5 = disassemble(s ++ " DIV", 
          removeFirstInstruction bytes)
      | BS.head bytes == 6 = disassemble(s ++ " TRUE", 
          removeFirstInstruction bytes)
      | BS.head bytes == 7 = disassemble(s ++ " FALSE", 
          removeFirstInstruction bytes)
      | BS.head bytes == 8 = disassemble(s ++ " GT", 
          removeFirstInstruction bytes)
      | BS.head bytes == 9 = disassemble(s ++ " LT", 
          removeFirstInstruction bytes)
      | BS.head bytes == 10 = disassemble(s ++ " NEQ", 
          removeFirstInstruction bytes)
      | BS.head bytes == 11 = disassemble(s ++ " EQ", 
          removeFirstInstruction bytes)
      | BS.head bytes == 12 = disassemble(s ++ " MINUS", 
          removeFirstInstruction bytes)
      | BS.head bytes == 13 = disassemble(s ++ " BANG", 
          removeFirstInstruction bytes)
      | BS.head bytes == 14 = disassemble(s ++ " JUMP " ++ (show (index bytes 1)),
          removeFirstInstruction(removeFirstInstruction bytes))
      | BS.head bytes == 15 = disassemble(s ++ " JUMPNT " ++ (show (index bytes 1)),
          removeFirstInstruction(removeFirstInstruction bytes))
      | BS.head bytes == 16 = disassemble(s ++ " SETGLOBAL " ++ (show (fromInteger(fromIntegral(index bytes 1)))),
          removeFirstInstruction(removeFirstInstruction bytes))
      | BS.head bytes == 17 = disassemble(s ++ " GETGLOBAL " ++ (show (fromInteger(fromIntegral(index bytes 1)))),
          removeFirstInstruction(removeFirstInstruction bytes))
      | BS.head bytes == 18 = disassemble(s ++ " ARRAY", 
          removeFirstInstruction bytes)
      | BS.head bytes == 19 = disassemble(s ++ " ARRAYEND", 
          removeFirstInstruction bytes)
      | BS.head bytes == 20 = disassemble(s ++ " HASH",
          removeFirstInstruction bytes)
      | BS.head bytes == 21 = disassemble(s ++ " HASHEND", 
          removeFirstInstruction bytes )
      | BS.head bytes == 22 = disassemble(s ++ " INDEX", 
          removeFirstInstruction bytes )
      | BS.head bytes == 23 = disassemble(s ++ " SETINDEX", 
          removeFirstInstruction bytes)
      | BS.head bytes == 24 = disassemble(s ++ " INDEXEND", 
          removeFirstInstruction bytes )
      | BS.head bytes == 25 = disassemble(s ++ " RETURNVALUE ", 
          removeFirstInstruction bytes)
      | BS.head bytes == 26 = disassemble(s ++ " OPRETURN ", 
          removeFirstInstruction bytes)
      | otherwise = error ("disassemble " ++ (show (BS.head bytes)))


removeFirstInstruction :: ByteString -> ByteString 
removeFirstInstruction b = 
  case BS.length b of 
    0 -> error "can't remove instruction of length 0?"
    1 -> BS.empty :: ByteString 
    _ -> pack(removeFirst(unpack b))

