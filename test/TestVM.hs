module TestVM where 

import VM 
import TestUtils


testVMOp1 :: String 
testVMOp1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("1 + 2 * 3 / 1 - 5")), []))

testVMOp2 :: String 
testVMOp2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("2 + 1 * 3 ")), []))

testVMOp3 :: String 
testVMOp3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("2 * 2 + 3 * 2")), []))

testVMOp4 :: String 
testVMOp4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 * 4 / 2 + 1")), []))

testVMOpBook1 :: String 
testVMOpBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("1 - 2")), []))

testVMOpBook2 :: String 
testVMOpBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("1 * 2")), []))

testVMOpBook3 :: String 
testVMOpBook3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("4 / 2")), []))

testVMOpBook4 :: String 
testVMOpBook4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("50 / 2 * 2 + 10 - 5")), []))

testVMOpBook5 :: String 
testVMOpBook5 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 + 5 + 5 + 5 - 10")), []))

testVMOpBook6 :: String 
testVMOpBook6 = parseStack(run(parseStatementToCompiled(parseStringToStatements("2 * 2 * 2 * 2 * 2")), []))

testVMOpBook7 :: String 
testVMOpBook7 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 * 2 + 10")), []))

testVMOpBook8 :: String 
testVMOpBook8 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 + 2 * 10")), []))

testVMOpBook9 :: String 
testVMOpBook9 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 * (2 + 10)")), []))

testVMOpBig :: String 
testVMOpBig = parseStack(run(parseStatementToCompiled(parseStringToStatements("70000 * 2")), []))

testVMPrefixMinus:: String 
testVMPrefixMinus= parseStack(run(parseStatementToCompiled(parseStringToStatements("-5")), []))

testVMPrefixBang :: String 
testVMPrefixBang = parseStack(run(parseStatementToCompiled(parseStringToStatements("!True")), []))

testVMIf:: String 
testVMIf= parseStack(run(parseStatementToCompiled(parseStringToStatements("if(True){5;}else{10;};")), []))

testVMElse:: String 
testVMElse= parseStack(run(parseStatementToCompiled(parseStringToStatements("if(False){5;}else{10;};")), []))

testVMNestedIf :: String 
testVMNestedIf = parseStack(run(parseStatementToCompiled(parseStringToStatements("if(True){if(True){3;}else{5;};};")), []))

testVMNestedElse :: String 
testVMNestedElse = parseStack(run(parseStatementToCompiled(parseStringToStatements("if(False){2;}else{if(False){3;}else{5;};};")), []))
