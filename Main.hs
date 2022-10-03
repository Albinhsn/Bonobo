module Main where

import Ast
import Lexer
import Parser
import Token

main = do
  let s = snd (parseTokens ("let five = 5;", []))
  print s
