module Main where

import Ast
import Lexer
import Parser
import Token

main = do
  let s =
        parseStatements
          ( snd (parseTokens ("let five = 5 / 5 + 5 * 5;", [])),
            []
          )

  print s
