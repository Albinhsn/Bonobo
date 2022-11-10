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
