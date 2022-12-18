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
  let k = parseStatementToCompiled(parseStringToStatements(""))
  file <- Prelude.readFile (args!!0)
  let c = parseStringToStatements(file)
  print (statementsToString c) 
  let a = parseStatementToCompiled(c) 
  let f = disassembleFunc("", a)
  print f
  -- let a = parseStringToStatements("fn fibonacci(number){if(number < 2){return number;}else{return fibonacci(number - 1) + add(2-3);};};")
  -- print c
  -- BS.writeFile "dis.bo" a
  -- print a
  -- let r = run a 
  -- mapM_ print r
