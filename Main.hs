module Main where

import Lexer

main = do
  let l = createLexer ";.=()"
  print l
  let l2 = readChar l
  let len = length (input l)
  let readPos = readPosition l
  print len
  print readPos
  print l2
