module Main where

import Ast
import Lexer
import Parser
import Token
import Utils
import ParserUtils
import Object 
import Eval




import Data.Typeable
import Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.Text as TS 
import Data.Text.Lazy as Tl 
import Data.ByteString.Lazy.UTF8 as BLU 
import Data.ByteString.UTF8 as BSU 
import Data.Text.Encoding as TSE 
import Data.Text.Lazy.Encoding as TLE 

-- main = do
  -- let tokens = (parseTokens (1, "print(\"Hello\", \"World!\");", []))
  -- -- print tokens
  -- let s = parseStatements (EXP, (getTokens(tokens), []))
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
main = do
  line <- Prelude.getLine 
  if Prelude.null line 
    then return ()
    else do
      putStrLn (show (BSU.fromString line))
      main 
