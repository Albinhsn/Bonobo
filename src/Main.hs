module Main where

import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object

import Data.ByteString as BS

main = do
  let s = parseStringToStatements("fn five(){return 5;};fn add(){let a = five(); return a;}; let c = add();") 
  -- print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  print ("Symbols: " ++ show(symbols a)) 
  print ("Funcs: " ++ Prelude.concat [inspectObject o ++ " " | o <- getFuncs(constants a)])
  print (disassemble ("", a))
  let r = run a 
  print r
  let k = parseStack(r)
  print k  





