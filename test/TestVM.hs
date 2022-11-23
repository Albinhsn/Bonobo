module TestVM where 

import VM 
import CompilerUtils
import Object
import Compiler


testVMOp1 :: String 
testVMOp1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 1 + 2 * 3 / 1 - 5"))))

testVMOp2 :: String 
testVMOp2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 2 + 1 * 3 "))))

testVMOp3 :: String 
testVMOp3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 2 * 2 + 3 * 2"))))

testVMOp4 :: String 
testVMOp4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 * 4 / 2 + 1"))))

testVMOpBook1 :: String 
testVMOpBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 1 - 2"))))

testVMOpBook2 :: String 
testVMOpBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 1 * 2"))))

testVMOpBook3 :: String 
testVMOpBook3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 4 / 2"))))

testVMOpBook4 :: String 
testVMOpBook4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 50 / 2 * 2 + 10 - 5"))))

testVMOpBook5 :: String 
testVMOpBook5 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 + 5 + 5 + 5 - 10"))))

testVMOpBook6 :: String 
testVMOpBook6 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 2 * 2 * 2 * 2 * 2"))))

testVMOpBook7 :: String 
testVMOpBook7 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 * 2 + 10"))))

testVMOpBook8 :: String 
testVMOpBook8 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 + 2 * 10"))))

testVMOpBook9 :: String 
testVMOpBook9 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 5 * (2 + 10)"))))

testVMStrBook :: String 
testVMStrBook = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = \"monkey\"")))) 

testVMOpStrBook1 :: String 
testVMOpStrBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = \"mon\" + \"key\"")))) 

testVMOpStrBook2 :: String 
testVMOpStrBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = \"mon\" + \"key\" + \"banana\"")))) 

testVMOpBig :: String 
testVMOpBig = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 70000 * 2"))))

testVMPrefixMinus:: String 
testVMPrefixMinus= parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = -5"))))

testVMPrefixBang :: String 
testVMPrefixBang = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = !True"))))

testVMIf:: String 
testVMIf= parseStack(run(parseStatementToCompiled(parseStringToStatements("if(True){let a = 5;}else{let b = 10;};"))))

testVMElse:: String 
testVMElse= parseStack(run(parseStatementToCompiled(parseStringToStatements("if(False){let a = 5;}else{let b = 10;};"))))

testVMNestedIf :: String 
testVMNestedIf = parseStack(run(parseStatementToCompiled(parseStringToStatements(" if(True){if(True){let a = 3;}else{let b = 5;};};"))))

testVMNestedElse :: String 
testVMNestedElse = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = 0; if(False){a = 2;}else{if(False){a = 3;}else{a = 5;};};"))))

testVMLetBook1 :: String 
testVMLetBook1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let one = 1; one;"))))

testVMLetBook2 :: String 
testVMLetBook2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let one = 1; let two = 2; two + one;"))))

testVMLetBook3 :: String 
testVMLetBook3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let one = 1; let two = one + one; two + one;"))))

testVMArray1 :: String 
testVMArray1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = []"))))

testVMArray2 :: String 
testVMArray2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [1, \"hi\"]"))))

testVMArray3 :: String 
testVMArray3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [[], [1,2]];"))))

testVMArray4 :: String 
testVMArray4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [[[1, 2]]];"))))

testVMArray5 :: String 
testVMArray5 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [[1,2], []];"))))

testVMMap1:: String 
testVMMap1= parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = {1:1, 2:2, 3:3, 4:4}"))))

testVMMap2:: String 
testVMMap2= parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = {1:{1:1, 2:2}}"))))

testVMAM1:: String 
testVMAM1= parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [{1:2}]"))))

testVMAM2:: String 
testVMAM2= parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = {1:[1,2]}"))))

testVMIndex1 :: String 
testVMIndex1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [[0,1],2]; let b = a[0][1];"))))

testVMIndex2 :: String 
testVMIndex2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = {0:{1:2}}; let b = a[0][1];"))))

testVMIndex3 :: String 
testVMIndex3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [{1:True}]; let b = a[0][1];"))))

testVMIndex4 :: String 
testVMIndex4 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = {0:[1,True]}; let b = a[0][1];"))))

testVMIndex5 :: String 
testVMIndex5 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [{\"a\":[0,{\"b\": True}]}]; let b = a[0][\"a\"][1][\"b\"];"))))

testVMIndex6 :: String 
testVMIndex6 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [0,1,True]; let b = a[12/6]"))))

testVMIndexAssign1 :: String 
testVMIndexAssign1 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = [[0,1],2]; a[0][1]= True;"))))

testVMIndexAssign2 :: String 
testVMIndexAssign2 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = {0:{1:False}}; a[0][1] = True;"))))

testVMIndexAssign3 :: String 
testVMIndexAssign3 = parseStack(run(parseStatementToCompiled(parseStringToStatements("let a = {0:[False, {0:[0, False]}]}; a[0][1][0][1] = True;"))))

testVMFN1 :: String 
testVMFN1 = inspectObject(getGlobal(1, run(parseStatementToCompiled(parseStringToStatements("fn add(){return 5 + 10;}; let a = add();")))))

testVMFN2 :: String 
testVMFN2 = inspectObject(getGlobal(1, run(parseStatementToCompiled(parseStringToStatements("fn add(){let a = 5; let b = 10; return 5 + 10;}; let c = add();")))))

testVMFN3 :: String 
testVMFN3 = inspectObject(getGlobal(1, run(parseStatementToCompiled(parseStringToStatements("fn add(a){return a;}; let c = add(2);")))))

testVMFN4 :: String 
testVMFN4 = inspectObject(getGlobal(1, run(parseStatementToCompiled(parseStringToStatements("fn add(){let five = 5; return 5;}; let a = add();")))))

testVMFN5 :: String 
testVMFN5 = inspectObject(getGlobal(1, run(parseStatementToCompiled(parseStringToStatements("fn add(a,b){return a + b;}; let c = add(2,3);")))))

