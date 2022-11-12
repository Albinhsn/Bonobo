module TestCode where 

import Code 
import TestUtils



testMake :: String 
testMake = parseMakeToPretty(parseStatementToCompiled(parseStringToStatements("1 + 2"))) 
    
testMake2 :: String 
testMake2 = disassemble("", parseStatementToCompiled(parseStringToStatements("2 * 2 + 3 * 2")))

testMakeTFTrue :: String 
testMakeTFTrue = prettyPrint(fst(parseStatementToCompiled(parseStringToStatements("True"))))

testMakeTFFalse :: String 
testMakeTFFalse = prettyPrint(fst(parseStatementToCompiled(parseStringToStatements("False"))))

testCodeIf:: String 
testCodeIf= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){5;};")))

testCodeIfElse:: String 
testCodeIfElse= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){5;}else{10;};")))

testCodeNestedIf:: String 
testCodeNestedIf= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){if(False){3;}else{5;};};")))

testCodeNestedElse:: String 
testCodeNestedElse= disassemble("", parseStatementToCompiled(parseStringToStatements("if(False){}else{if(False){3;}else{5;};};")))
