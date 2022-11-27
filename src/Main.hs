module Main where


import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object

import Data.ByteString as BS
main = do
  let s = parseStringToStatements("let a = 5 + 5 * 5;") 
  print s
  let c = statementsToString(s)
  print c
--   let a = parseStatementToCompiled s 
--   print a
--   print ("Symbols: " ++ show(symbols a)) 
--   print ("Funcs: " ++ Prelude.concat [inspectObject o ++ " " | o <- getFuncs(constants a)])
--   print (disassemble ("", a))
--   let r = run a 
--   -- print r
--   let k = parseStack(r)
--   print k  
-- -- let a = 50; fn sub(b){a = a - b;}; sub(20);

-- -- fn a(){fn b(){let a = 5; return a;}; let c = b(); return c;};let x = a();
