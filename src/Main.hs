module Main where

import Ast
import Lexer
import Parser
import Token
import Utils

main = do
  let s =
        snd
          ( parseStatements
              ( snd (parseTokens ("return 5;", [])),
                []
              )
          )
  print s
