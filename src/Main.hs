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


main = do
  let tokens = (parseTokens (1, "let a = {b[call()]:b[call()]}", []))
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
