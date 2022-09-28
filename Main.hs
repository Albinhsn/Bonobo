module Main where

import Lexer

main = do
  -- let l = createLexer ";,=()"
  let l = createLexer "albin ; ,=()"
  let s = getToken l
  print s
