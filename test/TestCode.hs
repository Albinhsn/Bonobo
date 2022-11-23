module TestCode where 

import Data.ByteString as BS

import VM
import Code 
import CompilerUtils
import Compiler
import Object



testMake :: String 
testMake = parseMakeToPretty(scopes(parseStatementToCompiled(parseStringToStatements("1 + 2")))!!0, constants (parseStatementToCompiled(parseStringToStatements("1 + 2"))))
    
testMake2 :: String 
testMake2 = disassemble("", parseStatementToCompiled(parseStringToStatements("2 * 2 + 3 * 2")))

testMakeTFTrue :: String 
testMakeTFTrue = prettyPrint(scopes(parseStatementToCompiled(parseStringToStatements("True")))!!0)

testMakeTFFalse :: String 
testMakeTFFalse = prettyPrint(scopes (parseStatementToCompiled(parseStringToStatements("False")))!!0)

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
testCodeArray4 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[[1, 2]]];")))

testCodeArray5 :: String 
testCodeArray5 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2], []];")))

testCodeMap1:: String 
testCodeMap1= disassemble("", parseStatementToCompiled(parseStringToStatements("let a = {1:1, 2:2, 3:3, 4:4}")))

testCodeIndex1 :: String 
testCodeIndex1 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2],0]; let b = a[0][1];")))

testCodeIndex2 :: String 
testCodeIndex2 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2],0]; a[0][1] = True;")))

testCodeIndex3 :: String 
testCodeIndex3 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2],0]; a[0] = True;")))

testCodeFN :: String 
testCodeFN = disassembleFunc ("", scopes( parseStatementToCompiled(parseStringToStatements("fn add(){return 5 + 10;};")))!!0)

testCodeFN2 :: String 
testCodeFN2 = disassembleFunc ("",scopes (parseStatementToCompiled(parseStringToStatements("fn add(a,b){return a + b;}; let c = add(1,2);")))!!0)

testCodeFN3 :: String 
testCodeFN3 = disassembleFunc ("",scopes (parseStatementToCompiled(parseStringToStatements("fn five(){return 5;};fn add(){return five();}; let c = add();")))!!0)
