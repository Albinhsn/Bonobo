module TestCode where 

import Code 
import TestUtils



testMake :: String 
testMake = parseMakeToPretty(parseStatementToCompiled(parseStringToStatements("1 + 2"))) 
    
testMake2 :: String 
testMake2 = prettyPrint(fst(parseStatementToCompiled(parseStringToStatements("2 * 2 + 3 * 2"))))
