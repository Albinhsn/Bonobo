module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils

main = do
  let tokens = (parseTokens ("let five = 5 + 5 == 5 < 5;", []))
  let s = parseStatements (snd tokens, [])
  print s 
  let a = statementToString (head (snd s))
  print a
