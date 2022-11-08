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


