module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
import ParserUtils
import Object 
import Eval
-- main = do
  -- -- print tokens
  -- print s 
  -- let a = statementsToString((snd(snd(s))))
  -- print a
  -- let b = evaluateProgram((snd(snd s)), ([], []))
  -- print b
  -- let c = concatContext(snd b)
  -- print c
  -- let v = "Vars: " ++ concat [inspectVariable x ++ ";" | x <- (fst(snd b))]
  -- print v  
  -- let f = "Funcs: " ++ concat [inspectFunction x ++ " " | x <- (snd(snd b))]
  -- print f 
evalMyShit :: String -> String 
evalMyShit s =  concatContext(snd (evaluateProgram((snd(snd (parseStatements(EXP, (getTokens(parseTokens (1, s, [])),[]))))), ([], []))))


main = do
  line <- getLine 
  if null line 
    then return ()
    else do
      putStrLn (evalMyShit line)
      main 
