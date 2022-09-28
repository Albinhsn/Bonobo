module Main where

import Lexer

main = do
  -- let l = createLexer ";,=()"
  let l = createLexer "a;\nlet"
  let s = getToken l
  print s
