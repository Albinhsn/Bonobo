module Main where

import Ast
import Lexer
import Parser
import Token
import Utils
import ParserUtils
import Object 
import Eval
import Code
import Compiler
import Utils2
import VM

import Data.Word (Word8)
import Data.ByteString.UTF8 as BSU 
import Data.ByteString as BS 
import Data.Map as DM
import Numeric (showHex)

main = do
  -- let tokens = (parseTokens (1, "1 + 2 + 6 + 3", []))
  -- -- print tokens
  -- let s = parseStatements (EXP, (getTokens(tokens), []))
  -- -- print s 
  -- let a = compile(snd(snd s), (BS.empty :: ByteString, []))
  -- let k = (prettyPrint (fst a) ++ " " ++ Prelude.concat [inspectObject x ++ " " | x  <- snd(a)])
  let s = parseStringToStatements("2 - 1 * 4 / 2")
  print (statementsToString s)
  let k = parseStack(run(parseStatementToCompiled s, []))
  print k  





-- main = do
--   let d = make (OPCONST, 65534)
--   print e
--   print (show d)
