module TestCode where 

import Data.ByteString as BS

import VM
import Code 
import CompilerUtils
import Compiler
import Object


testCodeIf:: String 
testCodeIf= disassemble("", parseStatementToCompiler(parseStringToStatements("if(True){let a = 5;};")))

testCodeIfElse:: String 
testCodeIfElse= disassemble("", parseStatementToCompiler(parseStringToStatements("if(True){let a = 5;}else{let b = 10;};")))

testCodeNestedIf:: String 
testCodeNestedIf= disassemble("", parseStatementToCompiler(parseStringToStatements("if(True){if(False){let a = 3;}else{let b = 5;};};")))

testCodeNestedElse:: String 
testCodeNestedElse= disassemble("", parseStatementToCompiler(parseStringToStatements("if(False){}else{if(False){let c = 3;}else{let b = 5;};};")))

testCodeLet:: String 
testCodeLet= disassemble("", parseStatementToCompiler(parseStringToStatements("let five = 5;")))

testCodeArray3 :: String 
testCodeArray3 = disassemble("", parseStatementToCompiler(parseStringToStatements("let a = [[], [1,2]];")))

testCodeArray4 :: String 
testCodeArray4 = disassemble("", parseStatementToCompiler(parseStringToStatements("let a = [[[1, 2]]];")))

testCodeArray5 :: String 
testCodeArray5 = disassemble("", parseStatementToCompiler(parseStringToStatements("let a = [[1,2], []];")))

testCodeMap1:: String 
testCodeMap1= disassemble("", parseStatementToCompiler(parseStringToStatements("let a = {1:1, 2:2, 3:3, 4:4}")))

testCodeIndex1 :: String 
testCodeIndex1 = disassemble("", parseStatementToCompiler(parseStringToStatements("let a = [[1,2],0]; let b = a[0][1];")))

testCodeIndex2 :: String 
testCodeIndex2 = disassemble("", parseStatementToCompiler(parseStringToStatements("let a = [[1,2],0]; a[0][1] = True;")))

testCodeIndex3 :: String 
testCodeIndex3 = disassemble("", parseStatementToCompiler(parseStringToStatements("let a = [[1,2],0]; a[0] = True;")))

testCodeFN :: String 
testCodeFN = disassembleFunc ("", parseStatementToCompiled(parseStringToStatements("fn add(){return 5 + 10;};")))

testCodeFN2 :: String 
testCodeFN2 = disassembleFunc ("",parseStatementToCompiled(parseStringToStatements("fn add(a,b){return a + b;}; let c = add(1,2);")))

testCodeFN3 :: String 
testCodeFN3 = disassembleFunc ("",parseStatementToCompiled(parseStringToStatements("fn five(){return 5;};fn add(){return five();}; let c = add();")))
