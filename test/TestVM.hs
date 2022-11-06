module TestVM where 

import VM 
import TestUtils


testVMOp :: String 
testVMOp = parseStack(run(parseStatementToCompiled(parseStringToStatements("1 + 2 * 3 / 1 - 5")), []))




