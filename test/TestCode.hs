module TestCode where 

import Code 
import Utils2



testMake :: String 
testMake = parseMakeToPretty(bytes (parseStatementToCompiled(parseStringToStatements("1 + 2"))), constants (parseStatementToCompiled(parseStringToStatements("1 + 2"))))
    
testMake2 :: String 
testMake2 = disassemble("", parseStatementToCompiled(parseStringToStatements("2 * 2 + 3 * 2")))

testMakeTFTrue :: String 
testMakeTFTrue = prettyPrint(bytes(parseStatementToCompiled(parseStringToStatements("True"))))

testMakeTFFalse :: String 
testMakeTFFalse = prettyPrint(bytes(parseStatementToCompiled(parseStringToStatements("False"))))

testCodeIf:: String 
testCodeIf= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){5;};")))

testCodeIfElse:: String 
testCodeIfElse= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){5;}else{10;};")))

testCodeNestedIf:: String 
testCodeNestedIf= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){if(False){3;}else{5;};};")))

testCodeNestedElse:: String 
testCodeNestedElse= disassemble("", parseStatementToCompiled(parseStringToStatements("if(False){}else{if(False){3;}else{5;};};")))

testCodeLet:: String 
testCodeLet= disassemble("", parseStatementToCompiled(parseStringToStatements("let five = 5; five;")))

testCodeArray1 :: String 
testCodeArray1 = disassemble("", parseStatementToCompiled(parseStringToStatements("[]")))

testCodeArray2 :: String 
testCodeArray2 = disassemble("", parseStatementToCompiled(parseStringToStatements("[1, \"hi\"]")))

testCodeArray3 :: String 
testCodeArray3 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[], [1,2]];")))

testCodeArray4 :: String 
testCodeArray4 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[[1, 2, ], ], ];")))
