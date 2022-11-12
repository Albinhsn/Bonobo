module TestPrebuilt where 


import Eval
import Utils2
import Ast
import Object

testEvalAppend:: String
testEvalAppend= concatContext(snd(evaluateProgram(parseStringToStatements "let a = []; a = append(a, 1);", ([], []))))

testEvalLenStr:: String
testEvalLenStr= concatContext(snd(evaluateProgram(parseStringToStatements "let s = \"hello world\"; let l = len(s);", ([], []))))

testEvalLenArray:: String
testEvalLenArray= concatContext(snd(evaluateProgram(parseStringToStatements "let a = [1,2,3,4]; let l = len(a);", ([], []))))

testEvalLenMap:: String
testEvalLenMap= concatContext(snd(evaluateProgram(parseStringToStatements "let a = {1:1,2:2,3:3,4:4}; let l = len(a);", ([], []))))


testPrint :: String
testPrint = concatContext(snd(evaluateProgram(parseStringToStatements "print(\"Hello, \", \"World!\")", ([], []))))
