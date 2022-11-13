module TestParser where

import Ast
import Lexer
import Parser
import Token
import Utils


testBasicLet:: String 
testBasicLet= 
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5; let five = \"five\"; let five = 5 + 5 + 5 ; let five = 5 / 5; let five = 5 * 5; let five = 5 + 5 * 5; let five = 5 * 5 + 5; let five = 5 - 5; let five = -5;", [])),
            []
          ))
        ))
      )
testMassiveAM :: String 
testMassiveAM= 
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let a = [{1:[{1:[]}]}]; let b = {1:[{1:[]}]};", [])),
            []
          ))
        ))
      )
testMassiveAM2:: String 
testMassiveAM2 = 
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let a = [{\"a\":[0,{\"b\": True}]}];", [])),
            []
          ))
        ))
      )



testReturnStatement :: String 
testReturnStatement =
  statementsToString(snd(snd(parseStatements (EXP,(getTokens(parseTokens (0, "return 5; return 5 + 5;", [])), [])))))
  
