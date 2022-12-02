module TestBool where

import Parser
import Ast 
import Lexer 
import Token 
import Utils 
import VM 
import CompilerUtils
import Compiler


testVMBool1 :: String 
testVMBool1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = True == True"))))

testVMBool2 :: String 
testVMBool2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = True == False"))))

testVMBool3 :: String 
testVMBool3= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 == 5"))))

testVMBool4 :: String 
testVMBool4 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 < 3"))))

testVMBool5 :: String 
testVMBool5 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 > 3"))))

testVMBool6 :: String 
testVMBool6 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 != 5"))))

testVMBool7 :: String 
testVMBool7 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = !True == False"))))

testVMBool8 :: String 
testVMBool8 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = -5 == 5"))))

testVMBoolBook1 :: String 
testVMBoolBook1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = (1 < 2) == True"))))

testVMBoolBook2 :: String 
testVMBoolBook2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = (1 > 2) == True"))))

testVMBoolBook3 :: String 
testVMBoolBook3 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = (10 + 50 + -5 - 5 * 2 / 2) < (100 - 35)"))))
