module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
import ParserUtils
import Object 
import Eval

-- let a = [[1],[2],[3]]; let b = a[0];
-- let a = [[1],[2],[3]]; let b = a[2][0];
-- let a = {1:{1:1}}; let b = a[1][1]

main = do
  let tokens = (parseTokens (1, "let a = [[0,1],1];a[0][1]= 0;", []))
  -- print tokens
  let s = parseStatements (EXP, (getTokens(tokens), []))
  -- print s 
  let a = statementsToString((snd(snd(s))))
  print a
  let b = evaluateProgram((snd(snd s)), ([], []))
  print b
  let c = concatContext(snd b)
  print c
  let v = "Vars: " ++ concat [inspectVariable x ++ ";" | x <- (fst(snd b))]
  print v  
  let f = "Funcs: " ++ concat [inspectFunction x ++ " " | x <- (snd(snd b))]
  print f 
