module Main where


import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object


main = do
  let x = "let five = 0; for(i = 0; i < 100000; i = i + 1;){five = five - i;}; print(five);"
  let s = parseStringToStatements(x) 
  -- print s
  let a = parseStatementToCompiler s 
  print "compiled"
  let r = run a
  mapM_ print r
