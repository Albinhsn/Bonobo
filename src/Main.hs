module Main where

import Utils2
import Utils
import VM
import Code

import Data.ByteString as BS

main = do
  let s = parseStringToStatements("fn add(a,b){return a + b;}; add(1,2);") 
  print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  print (disassemble ("", a))
  let k = parseStack(run a)
  putStrLn k  




