module TestBool where

import Parser
import Ast 
import Lexer 
import Token 
import Utils 
import VM 
import Utils2

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
testVMBool1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = True == True"))))

testVMBool2 :: String 
testVMBool2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = True == False"))))

testVMBool3 :: String 
testVMBool3= parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 == 5"))))

testVMBool4 :: String 
testVMBool4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 < 3"))))

testVMBool5 :: String 
testVMBool5 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 > 3"))))

testVMBool6 :: String 
testVMBool6 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 != 5"))))

testVMBool7 :: String 
testVMBool7 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = !True == False"))))

testVMBool8 :: String 
testVMBool8 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = -5 == 5"))))

testVMBoolBook1 :: String 
testVMBoolBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = (1 < 2) == True"))))

testVMBoolBook2 :: String 
testVMBoolBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = (1 > 2) == True"))))

testVMBoolBook3 :: String 
testVMBoolBook3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = (10 + 50 + -5 - 5 * 2 / 2) < (100 - 35)"))))
