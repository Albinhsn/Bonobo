module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
import ParserUtils
main = do
  let tokens = (parseTokens ("if(){}else{if(){if()", []))
  let s = parseStatements (EXP, (snd tokens, []))
  print s 
  let a = statementToString( head (snd(snd(s))))
  print a
