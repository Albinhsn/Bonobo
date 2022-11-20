module Main where

import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object

import Data.ByteString as BS

main = do
  let s = parseStringToStatements("fn add(a){return a;}; let c = add(2);") 
  -- print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  print ("Funcs: " ++ Prelude.concat [inspectObject o ++ " " | o <- getFuncs(constants a)])
  print (disassemble ("", a))
  let r = run a 
  -- print r
  let k = parseStack(r)
  print k  





