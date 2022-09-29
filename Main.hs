module Main where

import Lexer

main = do
  -- let l = createLexer ";,=()"
  -- let l = createLexer "a;\nlet"
  -- let s = getToken l
  let str = "albin  hsn"
  let str2 = "  hsn"
  let s2 = removeFirstChar str2
  print s2
  let s = parse (str, [])
  let a = [isLetter n | n <- str]
  -- let s = parse ("int", [])
  print a
  print s
