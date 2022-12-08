module Main where


import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object

import Data.ByteString as BS



main = do
  -- let x = "let d = 0; fn power(a){return a * a; }; for(i = 3; i < 6; i = i + 2;){d = d + power(i);}; print(d);"
  -- let x = "let d = 0; fn power(a){return a * a; }; fn add(){d = power(2);}; add(); print(d);"
  let x = "let d = 0; fn a(){return 5;}; for(i = 0; i < 3; i = i + 1;){d = d + a();}; print(d);"
  let s = parseStringToStatements(x) 
  -- print s
  let c = statementsToString(s)
  print c
  let a = parseStatementToCompiled s 
  -- print a
  let k = idk([], a) 
  let d = parseStatementToCompiler s 
  -- print d
  print (disassemble("", d))
  -- let f = runVM(VM{
  --         frames = [(0, a)],
  --         frameIndex = 0,
  --         bpOffset = 0, 
  --         global = [],
  --         stack = [],
  --         outputs =[]})
  -- print f
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
