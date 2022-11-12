module TestVM where 

import VM 
import Utils2


testVMOp1 :: String 
testVMOp1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("1 + 2 * 3 / 1 - 5"))))

testVMOp2 :: String 
testVMOp2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("2 + 1 * 3 "))))

testVMOp3 :: String 
testVMOp3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("2 * 2 + 3 * 2"))))

testVMOp4 :: String 
testVMOp4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 * 4 / 2 + 1"))))

testVMOpBook1 :: String 
testVMOpBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("1 - 2"))))

testVMOpBook2 :: String 
testVMOpBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("1 * 2"))))

testVMOpBook3 :: String 
testVMOpBook3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("4 / 2"))))

testVMOpBook4 :: String 
testVMOpBook4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("50 / 2 * 2 + 10 - 5"))))

testVMOpBook5 :: String 
testVMOpBook5 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 + 5 + 5 + 5 - 10"))))

testVMOpBook6 :: String 
testVMOpBook6 = parseStack(run(parseStatementToCompiled(parseStringToStatements("2 * 2 * 2 * 2 * 2"))))

testVMOpBook7 :: String 
testVMOpBook7 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 * 2 + 10"))))

testVMOpBook8 :: String 
testVMOpBook8 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 + 2 * 10"))))

testVMOpBook9 :: String 
testVMOpBook9 = parseStack(run(parseStatementToCompiled(parseStringToStatements("5 * (2 + 10)"))))

testVMStrBook :: String 
testVMStrBook = parseStack(run(parseStatementToCompiled(parseStringToStatements("\"monkey\"")))) 

testVMOpStrBook1 :: String 
testVMOpStrBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("\"mon\" + \"key\"")))) 

testVMOpStrBook2 :: String 
testVMOpStrBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("\"mon\" + \"key\" + \"banana\"")))) 

testVMOpBig :: String 
testVMOpBig = parseStack(run(parseStatementToCompiled(parseStringToStatements("70000 * 2"))))

testVMPrefixMinus:: String 
testVMPrefixMinus= parseStack(run(parseStatementToCompiled(parseStringToStatements("-5"))))

testVMPrefixBang :: String 
testVMPrefixBang = parseStack(run(parseStatementToCompiled(parseStringToStatements("!True"))))

testVMIf:: String 
testVMIf= parseStack(run(parseStatementToCompiled(parseStringToStatements("if(True){5;}else{10;};"))))

testVMElse:: String 
testVMElse= parseStack(run(parseStatementToCompiled(parseStringToStatements("if(False){5;}else{10;};"))))

testVMNestedIf :: String 
testVMNestedIf = parseStack(run(parseStatementToCompiled(parseStringToStatements("if(True){if(True){3;}else{5;};};"))))

testVMNestedElse :: String 
testVMNestedElse = parseStack(run(parseStatementToCompiled(parseStringToStatements("if(False){2;}else{if(False){3;}else{5;};};"))))

testVMLetBook1 :: String 
testVMLetBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let one = 1; one;"))))

testVMLetBook2 :: String 
testVMLetBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let one = 1; let two = 2; two + one;"))))

testVMLetBook3 :: String 
testVMLetBook3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let one = 1; let two = one + one; two + one;"))))

testVMArray1 :: String 
testVMArray1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("[]"))))

testVMArray2 :: String 
testVMArray2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("[1, \"hi\"]"))))

testVMArray3 :: String 
testVMArray3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [[], [1,2]];"))))

testVMArray4 :: String 
testVMArray4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [[[1, 2, ], ], ];"))))
