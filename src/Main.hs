module Main where


import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object

import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU      -- from utf8-string


main = do
  let s = parseStringToStatements("add(add(2,3,4),2,3);") 
--      
  print s

  let c = statementsToString(s)
  print c
  -- let a = parseStatementToCompiled s 
  -- -- print a
  -- print ("Symbols: " ++ show(symbols a)) 
  -- print ("Funcs: " ++ Prelude.concat [inspectObject o ++ " " | o <- getFuncs(constants a)])
  -- print (disassemble ("", a))
  -- let r = run a 
  -- -- print r
  -- let k = parseStack(r)
  -- print k  
