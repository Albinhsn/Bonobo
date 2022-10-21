module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
--  / ((-7 - 8))
main = do
  let tokens = (parseTokens ("return ((1 + 2) + 2);", []))
  let s = parseStatements (snd tokens, [])
  print s 

  let a = statementToString (head (snd s))
  print a
