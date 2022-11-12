module Utils2 where


import Object 
import Ast
import Compiler
import Code 
import Token
import Lexer 
import Utils
import Parser
import Compiler
import VM


import Data.ByteString as BS
import Data.Map

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
    bytes = BS.empty :: ByteString,
    constants = [],
    symbols = []
  })


parseStack ::  VM -> String 
parseStack v = ("Stack: " ++ Prelude.concat [inspectObject x | x <- (stack v)] ++ " " ++ "Globals: " ++ Prelude.concat [inspectGlobal x | x <- (global v)])

inspectGlobal :: (Int, Object) -> String 
inspectGlobal (i, o) = (show i) ++ " = " ++ inspectObject o ++ " "


disassemble :: (String, Compiler) -> String 
disassemble (s,c)= str 
  where 
    str 
      | BS.null (bytes c) = s
      | BS.head (bytes c) == 0 = disassemble(s ++ " CONST " ++  inspectObject (constants c!!(read(show (index (bytes c) 1)))), Compiler{
          bytes = removeFirstInstruction(removeFirstInstruction(bytes c)),
          constants = constants c, 
          symbols = symbols c 
        })
      --Pop
      | BS.head (bytes c) == 1 = disassemble(s ++ " POP", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 2 = disassemble(s ++ " ADD", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 3 = disassemble(s ++ " SUB", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 4 = disassemble(s ++ " MUL", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 5 = disassemble(s ++ " DIV", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 6 = disassemble(s ++ " TRUE", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 7 = disassemble(s ++ " FALSE", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 8 = disassemble(s ++ " GT", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 9 = disassemble(s ++ " LT", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 10 = disassemble(s ++ " NEQ", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 11 = disassemble(s ++ " EQ", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 12 = disassemble(s ++ " MINUS", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 13 = disassemble(s ++ " BANG", Compiler{
          bytes = removeFirstInstruction (bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 14 = disassemble(s ++ " JUMP " ++ (show (index (bytes c) 1)),Compiler{ 
          bytes = removeFirstInstruction(removeFirstInstruction(bytes c)),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 15 = disassemble(s ++ " JUMPNT " ++ (show (index (bytes c) 1)),Compiler{ 
          bytes = removeFirstInstruction(removeFirstInstruction(bytes c)),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 16 = disassemble(s ++ " SETGLOBAL " ++ (fst(symbols c !! (fromInteger(fromIntegral(index (bytes c) 1))))),Compiler{ 
          bytes = removeFirstInstruction(removeFirstInstruction(bytes c)),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 17 = disassemble(s ++ " GETGLOBAL " ++ (fst(symbols c !! (fromInteger(fromIntegral(index (bytes c) 1))))),Compiler{ 
          bytes = removeFirstInstruction(removeFirstInstruction(bytes c)),
          constants = constants c, 
          symbols = symbols c 
        })
      | BS.head (bytes c) == 18 = disassemble(s ++ " ARRAY", Compiler{ 
          bytes = removeFirstInstruction(bytes c),
          constants = constants c, 
          symbols = symbols c 
        })
      | otherwise = error ("disassemble" ++ (show (BS.head (bytes c))))
