module TestEval where 



import Eval 
import TestUtils
import Ast
import Object

testEvalFuncCall:: String
testEvalFuncCall= concatContext(snd(evaluateProgram(parseStringToStatements "fn add(a,b){return a + b;} let five = add(2,3);", ([], []))))

testEvalMulFunc :: String
testEvalMulFunc= concatContext(snd(evaluateProgram(parseStringToStatements "fn add(a,b){return a + b;} fn divide(a,b){return a / b;}", ([], []))))

testEvalMulVar:: String
testEvalMulVar= concatContext(snd(evaluateProgram(parseStringToStatements "let five = 5; let ten = 10;", ([], []))))

testEvalStringAdd :: String 
testEvalStringAdd = concatContext(snd(evaluateProgram(parseStringToStatements "let hello = \"Hello\"; let world = \" World!\"; let add = hello + world", ([], [])))) 

testEvalContext:: String
testEvalContext= concatContext(snd(evaluateProgram(parseStringToStatements "fn add(){let five = 5; return 3;}; let three = add();", ([], []))))

testEvalOverwriteVar:: String
testEvalOverwriteVar= concatContext(snd(evaluateProgram(parseStringToStatements "let five = 3; five = 5;", ([], []))))

testEvalNestedIf:: String
testEvalNestedIf= concatContext(snd(evaluateProgram(parseStringToStatements "if(5 > 3){if(5 < 3){let three = 3;}else{let five = 5;}}", ([], []))))

testEvalNestedElse:: String
testEvalNestedElse= concatContext(snd(evaluateProgram(parseStringToStatements "if(5 < 3){let three = 3;}else{if(5 < 3){let four = 4;}else{if(5 > 3){let five = 5;};};};", ([], []))))

testEvalArray:: String
testEvalArray= concatContext(snd(evaluateProgram(parseStringToStatements "fn add(a,b){return a +b};let arr = [1,\"Hi\", (2+3), add(2,3), True]", ([], []))))

testEvalArrayArray:: String
testEvalArrayArray= concatContext(snd(evaluateProgram(parseStringToStatements "let a = [1,2,3]; let arr = [a[2], 1];", ([], []))))
