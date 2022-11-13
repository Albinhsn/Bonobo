module TestObject where 


import Eval 
import Utils2
import Ast
import Object

-- testObj:: String
-- testObj = concatContext(snd(evaluateProgram(parseStringToStatements "let one = \"five\"; let two = True; let three = False; let four = 5; let five = !True; let six = -5;", ([], []))))

-- testObjMinus:: String
-- testObjMinus= inspectVariable(head(fst(snd(evaluateProgram(parseStringToStatements "", ([],[]))))))


-- testObjOp:: String
-- testObjOp = concatContext(snd(evaluateProgram(parseStringToStatements "let zero = 5 - 5; let one = 5 + 5; let two = 5 * 5; let three = 5 / 5;", ([], []))))


-- testObjBoolOp:: String
-- testObjBoolOp = concatContext(snd(evaluateProgram(parseStringToStatements "let one = 5 > 5; let two = 3 < 5; let three = 5 == 5; let four = 5 != 5;", ([], []))))

-- testObjFunc:: String
-- testObjFunc= inspectFunction(head(snd(snd(evaluateProgram(parseStringToStatements "fn add(a,b){return a + b;};", ([],[]))))))
