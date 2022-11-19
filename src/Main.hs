module Main where

import Utils2
import Utils
import VM
import Code

import Data.ByteString as BS

main = do
  let s = parseStringToStatements("fn add(a,b){return a + b;}; let c = add(2,3);") 
  -- print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  print (disassemble ("", a))
  -- let k = parseStack(run a)
  -- print k  




