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

  -- let a = compile(snd(snd s), (BS.empty :: ByteString, []))

parseStack :: [Object] -> String 
parseStack o = Prelude.concat [inspectObject x | x <- o]
