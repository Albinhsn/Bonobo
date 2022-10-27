module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
import ParserUtils
main = do
  let tokens = (parseTokens (1, "5", []))
  print tokens
  let s = parseStatements (EXP, (getTokens(tokens), []))
  print s 
  let a = statementToString( head (snd(snd(s))))
  print a

