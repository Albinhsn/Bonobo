module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
main = do
  
  let tokens = (parseTokens ("five((5 + 4) * 3)", []))
  print tokens
  print ""
  let s = parseStatements (EXP, (snd tokens, []))
  print s 
  print ""
  let a = statementToString (head (snd(snd s)))
  print a
