module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils

main = do
  let s =
          ( parseStatements
              ( snd (parseTokens ("let five = 5 + 5 + 5;", [])),
                []
              )
          )
  print s 
  let a = statementToString (head (snd s))
  print a
