module TestObject where 


import Eval 
import TestUtils
import Ast
import Object

testObjTrue:: String
testObjTrue= inspectObject(evaluateExpression(expression(last (parseStringToStatements "let five = true;"))))
testObjFalse:: String
testObjFalse= inspectObject(evaluateExpression(expression(last (parseStringToStatements "let five = false;"))))
testObjInt:: String
testObjInt= inspectObject(evaluateExpression(expression(last (parseStringToStatements "let five = 5;"))))
