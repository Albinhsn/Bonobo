module Main where

import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object

import Data.ByteString as BS

main = do
  let s = parseStringToStatements("fn add(f,s){fn sub(a,b){let c = a - b;return c;}; let d = sub(f,s); return d;};let c = add(10, 5);") 
  -- print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  print ("Symbols: " ++ show(symbols a)) 
  print ("Funcs: " ++ Prelude.concat [inspectObject o ++ " " | o <- getFuncs(constants a)])
  print (disassemble ("", a))
  let r = run a 
  -- print r
  let k = parseStack(r)
  print k  
