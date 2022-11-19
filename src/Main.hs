module Main where

import Utils2
import Utils
import VM
import Code

import Data.ByteString as BS

main = do
-- fn add(a,b){return a + b;}; let a = add(1,2);
  let s = parseStringToStatements("let a = 5; fn add(){return a;};  let b = add();") 
  -- print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  print (disassemble ("", a))
  let k = parseStack(run a)
  putStrLn k  




