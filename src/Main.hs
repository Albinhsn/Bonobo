module Main where


import CompilerUtils
import Compiler
import Utils
import VM
import Code
import Object
import Lexer
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU
import System.Environment

main = do
  args <- getArgs
  -- let k = parseStatementToCompiled(parseStringToStatements("let a = 5; for(i = 5; i < 10; i = i + 2;){a = a + i;}; print(a);"))
  file <- Prelude.readFile (args!!0)
  let a = parseStatementToCompiled(parseStringToStatements(file)) 
  -- print c
  BS.writeFile "dis.bo" a
  -- b <- BS.readFile "a.out"
  -- -- let z = b == a 
  -- -- print z
  -- -- print a
  let r = run a 
  mapM_ print r
