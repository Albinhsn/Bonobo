module TestBool where

import Parser
import Ast 
import Lexer 
import Token 
import Utils 
import VM 
import TestUtils

testBools:: String
testBools=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 == 5; let five = 5 == 5 + 5; let five = 5 == 5 + 5 * 5; let five = 5 == 5 + -5; let five = 5 == 5 + -5 * -5; let five = 5 > 5 * 2 == 2 + -3 * 5 > 1; let five = b[0] == b[0];", [])),
            []
          ))
        )
      ))

testVMBool1 :: String 
testVMBool1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("True == True")), []))

testVMBool2 :: String 
testVMBool2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("True == False")), []))

testVMBool3 :: String 
testVMBool3= parseStack(run(parseStatementToCompiled(parseStringToStatements("5 == 5")), []))

testVMBool4 :: String 
testVMBool4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 < 3")), []))

testVMBool5 :: String 
testVMBool5 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 > 3")), []))

testVMBool6 :: String 
testVMBool6 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 != 5")), []))

testVMBool7 :: String 
testVMBool7 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 > True")), []))
