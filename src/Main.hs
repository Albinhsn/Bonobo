module Main where

import Utils2
import Utils
import VM
import Code

import Data.ByteString as BS

main = do
-- let a = [[0,1],2]; a[0][1]= True;
  let s = parseStringToStatements("let a = [[0,1],2]; a[0][1]= True;") 
  -- print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  print (disassemble ("", a))
  let k = parseStack(run a)
  putStrLn k  




