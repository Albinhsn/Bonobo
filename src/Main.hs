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
  -- let s = parseStringToStatements("if(5 < 3){let three = 3;}else{if(5 < 3){let four = 4;}else{if(5 > 3){let five = 5;};")
  let tokens = (parseTokens (1, "if(5 < 3){let three = 3;}else{if(5 < 3){let four = 4;}else{if(5 > 3){let five = 5;};", []))
  -- print tokens
  let s = parseStatements (EXP, (getTokens(tokens), []))
  -- print s
  print (statementsToString (snd (snd s)))
  let a = findLastBlockType(fst (s), Prelude.last(snd(snd  s)))
  print a
  -- let a = parseStatementToCompiled s
  -- print (disassemble ("", a))
  -- let k = parseStack(run(a, []))
  -- print k  





-- main = do
--   let d = make (OPCONST, 65534)
--   print e
--   print (show d)



  -- let s = parseStatements (EXP, (getTokens(tokens), []))
  -- print s 
  -- let a = compile(snd(snd s), (BS.empty :: ByteString, []))
  -- let k = (prettyPrint (fst a) ++ " " ++ Prelude.concat [inspectObject x ++ " " | x  <- snd(a)])
