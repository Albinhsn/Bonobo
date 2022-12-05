module Main where


import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object

import Data.ByteString as BS



main = do
  let x = "for(i = 0; i < 5; i = i + 1;){let five = 5;};"
  let s = parseStringToStatements(x) 
  -- print s
  let c = statementsToString(s)
  -- print c
  let a = parseStatementToCompiled s 
  let k = idk([], a) 
  print (k !! 30)
  print k
  let d = parseStatementToCompiler s 
  print (disassemble("", d))
  -- print a
  -- print (disassemble ("", a))
  let r = run a
  mapM_ print r
-- import System.Environment 
-- import Data.ByteString.UTF8 as BSU 
  -- args <- getArgs 
  -- -- print f 
  -- x <- readFile (args!!0)
  -- let s = parseStringToStatements(s)
  -- -- print s
  -- let dis = (args!!1) == "--dis" 
  -- print c
  -- let a = parseStatementToCompiled s 
  -- writeFile "a.out" (BSU.toString(scopes a !! 0))
  -- print a
  -- print ("Symbols: " ++ show(symbols a)) 
  -- print (disassemble ("", a))
  -- -- y <- readFile "a.out"
  -- -- let r = run (BSU.fromString (scopes a !! 0)) 
  -- let r = run (scopes a !! 0) 
  -- mapM_ print r
  -- mapM_ print r
  -- putStrLn "" 
