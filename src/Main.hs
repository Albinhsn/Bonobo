module Main where


import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object
import System.Environment 
import Data.ByteString.UTF8 as BSU 

main = do
  args <- getArgs 
  -- print f 
  x <- readFile (args!!0)
  let s = parseStringToStatements(x) 
  -- print s
  let dis = (args!!1) == "--dis" 
  let c = statementsToString(s)
  -- print c
  let a = parseStatementToCompiled s 
  writeFile "a.out" (BSU.toString(scopes a !! 0))
  -- print a
  -- print ("Symbols: " ++ show(symbols a)) 
  -- print (disassemble ("", a))
  y <- readFile "a.out"
  let r = run (BSU.fromString y) 
  mapM_ print r
  putStrLn "" 
