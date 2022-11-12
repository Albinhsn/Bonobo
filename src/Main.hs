module Main where

import Utils2
import Utils
import VM

main = do
  let s = parseStringToStatements("if(True){5;}else{10;};") 
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s
  print (disassemble ("", a))
  let k = parseStack(run(a, []))
  print k  




