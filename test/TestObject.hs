module TestObject where 


import Eval 
import TestUtils
import Ast
import Object

testObjTrue:: String
testObjTrue= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = True;", ([], []))))))

testObjString:: String
testObjString = inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = \"five\";", ([], []))))))

testObjFalse:: String
testObjFalse= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = False;", ([],[]))))))

testObjInt:: String
testObjInt= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5;", ([],[]))))))


testObjBang:: String
testObjBang= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = !True;", ([],[]))))))

testObjMinus:: String
testObjMinus= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = -5;", ([],[]))))))


testObjOpMinus:: String
testObjOpMinus= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5 - 5;", ([],[]))))))

testObjOpPlus:: String
testObjOpPlus= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5 + 5;", ([],[]))))))

testObjOpMul:: String
testObjOpMul= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5 * 5;", ([], []))))))

testObjOpDiv :: String
testObjOpDiv= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5 / 5;", ([],[]))))))


testObjOpGT :: String
testObjOpGT= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5 > 5;", ([],[]))))))

testObjOpLT :: String
testObjOpLT= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 3 < 5;", ([],[]))))))

testObjOpEQ :: String
testObjOpEQ= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5 == 5;", ([],[]))))))

testObjOpNEQ :: String
testObjOpNEQ= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 5 != 5;", ([],[]))))))

testObjGrouped :: String
testObjGrouped= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "let five = 2 * (5 + 10);", ([],[]))))))

testObjIfFalse:: String
testObjIfFalse= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "if(5 < 3){let ten = 10;}else{let five = 5;}", ([],[]))))))

testObjIfTrue:: String
testObjIfTrue= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "if(5 > 3){let ten = 10;}else{let five = 5;}", ([],[]))))))

testObjFunc:: String
testObjFunc= inspectFunction(head(snd(snd(evaluateProgram(parseStringToStatements "fn add(a,b){return a + b;}", ([],[]))))))
