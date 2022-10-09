module Main where

import Ast
import Lexer
import Parser
import Token
import Utils

main = do
  let s =
        parseStatements
          ( snd (parseTokens ("let five = 5;", [])),
            []
          )
  printStatement (head (snd s))
