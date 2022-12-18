module TestVM where 

import VM 
import CompilerUtils
import Object
import Compiler


testVMOp1 :: String 
testVMOp1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 1 + 2 * 3 / 1 - 5"))))

testVMOp2 :: String 
testVMOp2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 2 + 1 * 3 "))))

testVMOp3 :: String 
testVMOp3 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 2 * 2 + 3 * 2"))))

testVMOp4 :: String 
testVMOp4 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 * 4 / 2 + 1"))))

testVMOpBook1 :: String 
testVMOpBook1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 1 - 2"))))

testVMOpBook2 :: String 
testVMOpBook2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 1 * 2"))))

testVMOpBook3 :: String 
testVMOpBook3 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 4 / 2"))))

testVMOpBook4 :: String 
testVMOpBook4 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 50 / 2 * 2 + 10 - 5"))))

testVMOpBook5 :: String 
testVMOpBook5 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 + 5 + 5 + 5 - 10"))))

testVMOpBook6 :: String 
testVMOpBook6 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 2 * 2 * 2 * 2 * 2"))))

testVMOpBook7 :: String 
testVMOpBook7 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 * 2 + 10"))))

testVMOpBook8 :: String 
testVMOpBook8 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 + 2 * 10"))))

testVMOpBook9 :: String 
testVMOpBook9 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 5 * (2 + 10)"))))

testVMStrBook :: String 
testVMStrBook = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = \"monkey\"")))) 

testVMOpStrBook1 :: String 
testVMOpStrBook1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = \"mon\" + \"key\"")))) 

testVMOpStrBook2 :: String 
testVMOpStrBook2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = \"mon\" + \"key\" + \"banana\"")))) 

testVMOpBig :: String 
testVMOpBig = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 70000 * 2"))))

testVMPrefixMinus:: String 
testVMPrefixMinus= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = -5"))))

testVMPrefixBang :: String 
testVMPrefixBang = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = !True"))))

testVMIf:: String 
testVMIf= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("if(True){let a = 5;}else{let b = 10;};"))))

testVMElse:: String 
testVMElse= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("if(False){let a = 5;}else{let b = 10;};"))))

testVMNestedIf :: String 
testVMNestedIf = parseStack(runTest(parseStatementToCompiled(parseStringToStatements(" if(True){if(True){let a = 3;}else{let b = 5;};};"))))

testVMNestedElse :: String 
testVMNestedElse = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = 0; if(False){a = 2;}else{if(False){a = 3;}else{a = 5;};};"))))

testVMLetBook1 :: String 
testVMLetBook1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let one = 1;"))))

testVMLetBook2 :: String 
testVMLetBook2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let one = 1; let two = 2; let three = two + one;"))))

testVMLetBook3 :: String 
testVMLetBook3 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let one = 1; let two = one + one; let three = two + one;"))))

testVMArray1 :: String 
testVMArray1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = []"))))

testVMArray2 :: String 
testVMArray2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [1, \"hi\"]"))))

testVMArray3 :: String 
testVMArray3 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [[], [1,2]];"))))

testVMArray4 :: String 
testVMArray4 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [[[1, 2]]];"))))

testVMArray5 :: String 
testVMArray5 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [[1,2], []];"))))

testVMMap1:: String 
testVMMap1= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = {1:1, 2:2, 3:3, 4:4}"))))

testVMMap2:: String 
testVMMap2= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = {1:{1:1, 2:2}}"))))

testVMAM1:: String 
testVMAM1= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [{1:2}]"))))

testVMAM2:: String 
testVMAM2= parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = {1:[1,2]}"))))

testVMIndex1 :: String 
testVMIndex1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [[0,1],2]; let b = a[0][1];"))))

testVMIndex2 :: String 
testVMIndex2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = {0:{1:2}}; let b = a[0][1];"))))

testVMIndex3 :: String 
testVMIndex3 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [{1:True}]; let b = a[0][1];"))))

testVMIndex4 :: String 
testVMIndex4 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = {0:[1,True]}; let b = a[0][1];"))))

testVMIndex5 :: String 
testVMIndex5 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [{\"a\":[0,{\"b\": True}]}]; let b = a[0][\"a\"][1][\"b\"];"))))

testVMIndex6 :: String 
testVMIndex6 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [0,1,True]; let b = a[12/6]"))))

testVMIndexAssign1 :: String 
testVMIndexAssign1 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = [[0,1],2]; a[0][1]= True;"))))

testVMIndexAssign2 :: String 
testVMIndexAssign2 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = {0:{1:False}}; a[0][1] = True;"))))

testVMIndexAssign3 :: String 
testVMIndexAssign3 = parseStack(runTest(parseStatementToCompiled(parseStringToStatements("let a = {0:[False, {0:[0, False]}]}; a[0][1][0][1] = True;"))))

testVMFN1 :: String 
testVMFN1 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(){return 5 + 10;}; let a = add();")))))

testVMFN2 :: String 
testVMFN2 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(){let a = 5; let b = 10; return 5 + 10;}; let c = add();")))))

testVMFN3 :: String 
testVMFN3 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(a){return a;}; let c = add(2);")))))

testVMFN4 :: String 
testVMFN4 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(){let five = 5; return 5;}; let a = add();")))))

testVMFN5 :: String 
testVMFN5 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(a,b){return a + b;}; let c = add(2,3);")))))

testVMFN6 :: String 
testVMFN6 = inspectObject(getGlobal(5, runTest(parseStatementToCompiled(parseStringToStatements("fn five(){return 5;};fn add(){return five();}; let c = add();")))))

testVMFN7 :: String 
testVMFN7 = inspectObject(getGlobal(5, runTest(parseStatementToCompiled(parseStringToStatements("fn five(){return 5;};fn add(){let a = five(); return a;}; let c = add();")))))

testVMFN9 :: String 
testVMFN9 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn f(){fn l(){return 5;}; return l();}; let c = f();")))))

testVMFN10 :: String 
testVMFN10 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(){let a = [0,1,2,3]; return a[2];}; let c = add();")))))

testVMFN11 :: String 
testVMFN11 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(b){let a = [0,1,2,3]; return a[b];}; let c = add(2);")))))

testVMFN12 :: String 
testVMFN12 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(){let a = {2:True}; return a[2];}; let c = add();")))))

testVMFN13 :: String 
testVMFN13 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(b){let a = {2:True}; return a[b];}; let c = add(2);")))))

testVMFN14 :: String 
testVMFN14 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(f,s,t,p){let a = {0:[False, {0:[0, False]}]}; return a[f][s][t][p];}; let c = add(0,1,0,1);")))))

testVMFN15 :: String 
testVMFN15 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(f,s){fn div(a,b){let d = a / b; return d;}; fn sub(a,b){let d = a - b; return div(a,d);};let a = sub(f,s); return a;};let c = add(10, 5);")))))

testVMFN16 :: String 
testVMFN16 = inspectObject(getGlobal(4, runTest(parseStatementToCompiled(parseStringToStatements("fn add(f,s){fn sub(a,b){let c = a - b;return c;}; let d = sub(f,s); return d;};let c = add(10, 5);")))))

testVMFNBook:: String 
testVMFNBook= inspectObject(getGlobal(6, runTest(parseStatementToCompiled(parseStringToStatements("let globalSeed = 50; fn minusOne(){let num = 1; return globalSeed-num;}; fn minusTwo() {let num = 2; return globalSeed - num;}; let res = minusOne() + minusTwo();")))))

testVMFN17 :: String 
testVMFN17 = inspectObject(getGlobal(3, runTest(parseStatementToCompiled(parseStringToStatements("let e = 50; fn a(){let c = 10; fn b(){let d = 2; c = c - d;}; b(); e = e - c;}; a();")))))

testVMFN18 :: String 
testVMFN18 = inspectObject(getGlobal(3, runTest(parseStatementToCompiled(parseStringToStatements("let a = 50; fn sub(){let c = 20; a = a - c;}; sub();")))))
