module Utils2 where


import Object 
import Data.ByteString as BS
import Ast
import Compiler
import Code 
import Token
import Lexer 
import Utils
import Parser
import Compiler
import VM

reverseList [] = []
reverseList xs = Prelude.last xs : reverseList (Prelude.init xs)

parseStringToStatementsString :: String -> String 
parseStringToStatementsString s =  statementsToString(snd(snd( parseStatements(EXP,( getTokens(parseTokens(0, s, [])),[])))))

parseStringToStatements :: String -> [Statement] 
parseStringToStatements s = snd(snd( parseStatements(EXP,( getTokens(parseTokens(0, s, [])),[]))))


parseMakeToPretty :: (ByteString, [Object]) -> String 
parseMakeToPretty (b, o) = (prettyPrint b ++ " - " ++ Prelude.concat [inspectObject x ++ " " | x  <- o])

parseStatementToCompiled :: [Statement] -> (ByteString, [Object])
parseStatementToCompiled s = compile(s, (BS.empty :: ByteString, []))


parseStack :: [Object] -> String 
parseStack o = Prelude.concat [inspectObject x | x <- o]

disassemble :: (String, (ByteString, [Object])) -> String 
disassemble (s,(b,o))= str 
  where 
    str 
      | BS.null b = s
      | BS.head b == 0 = disassemble(s ++ " CONST " ++  inspectObject (o!!(read(show (index b 1)))), (removeFirstInstruction(removeFirstInstruction b), o))
      --Pop
      | BS.head b == 1 = disassemble(s ++ " POP", (removeFirstInstruction b, o))
      | BS.head b == 2 = disassemble(s ++ " ADD", (removeFirstInstruction b, o))
      | BS.head b == 3 = disassemble(s ++ " SUB", (removeFirstInstruction b, o))
      | BS.head b == 4 = disassemble(s ++ " MUL", (removeFirstInstruction b, o))
      | BS.head b == 5 = disassemble(s ++ " DIV", (removeFirstInstruction b, o))
      | BS.head b == 6 = disassemble(s ++ " TRUE", (removeFirstInstruction b, o)) 
      | BS.head b == 7 = disassemble(s ++ " FALSE", (removeFirstInstruction b, o))  
      | BS.head b == 8 = disassemble(s ++ " GT", (removeFirstInstruction b, o))
      | BS.head b == 9 = disassemble(s ++ " LT", (removeFirstInstruction b, o))   
      | BS.head b == 10 = disassemble(s ++ " NEQ", (removeFirstInstruction b, o))     
      | BS.head b == 11 = disassemble(s ++ " EQ", (removeFirstInstruction b, o))      
      | BS.head b == 12 = disassemble(s ++ " MINUS", (removeFirstInstruction b, o))      
      | BS.head b == 13 = disassemble(s ++ " BANG", (removeFirstInstruction b, o))
      | BS.head b == 14 = disassemble(s ++ " JUMP " ++ (show (index b 1)), (removeFirstInstruction (removeFirstInstruction b), o))
      | BS.head b == 15 = disassemble(s ++ " JUMPNT " ++ (show (index b 1)), (removeFirstInstruction (removeFirstInstruction b), o))
      | otherwise = error "disassemble"
