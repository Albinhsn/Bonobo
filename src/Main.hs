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



main = do
  -- let x = "let five = 5; fn add(a){return a + five;}; for(i = 0; i < 100000; i = i + 1;){five = five + i;}; print(five);" 
  let s = parseStringToStatements("let five =[0,1,2,3,4]; for(i = 0; i < 5; i = i + 1;){print(five[i]);};") 
  -- print (statementsToString s)
  let a = parseStatementToCompiled s 
  -- let c = disassembleFunc("", a)
  -- print c
  -- -- BS.writeFile "a.out" a
  -- -- b <- BS.readFile "a.out"
  -- -- let z = b == a 
  -- -- print z
  -- -- print a
  let r = run a 
  mapM_ print r
