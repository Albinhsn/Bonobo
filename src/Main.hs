module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils

main = do
  let s =
        snd
          ( parseStatements
              ( snd (parseTokens ("let five = -5;", [])),
                []
              )
          )
  let a = statementToString (head s)
  -- print s
  print a
