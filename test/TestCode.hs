module TestCode where 

import Code 
import TestUtils



testMake :: String 
testMake = parseMakeToPretty(parseStatementToCompiled(parseStringToStatements("1 + 2"))) 
    
