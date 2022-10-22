module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
main = do
  
  let tokens = (parseTokens ("five = -5;", []))
  print tokens
  let s = parseStatements (EXP, (snd tokens, []))
  print s 

  let a = statementToString (head (snd(snd s)))
  print a
