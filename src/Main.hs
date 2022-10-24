module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
import ParserUtils
main = do
  let tokens = (parseTokens ("if(5==5){if(5 == 5){if(5 == 5){}};}", []))
  let b = [Statement {statementType = IFSTA, statementUni = IfStatement {closedCon = False, con = [Statement {statementType = IFSTA, statementUni = IfStatement {closedCon = False, con = [], alt = [], closedAlt = False}, expression = GroupedExpression {expressionType = GROUPEDEXP, groupedExpression = BoolExpression {expressionType = BOOLEXP, leftBool = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = "5"}, boolOperator = Token {typ = EQUALS, literal = "=="}, rightBool = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = "5"}}, closed = True}}], alt = [], closedAlt = False}, expression = GroupedExpression {expressionType = GROUPEDEXP, groupedExpression = BoolExpression {expressionType = BOOLEXP, leftBool = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = "5"}, boolOperator = Token {typ = EQUALS, literal = "=="}, rightBool = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = "5"}}, closed = True}}]
  let s = parseStatements (EXP, (snd tokens, []))
  print s 
  let a = statementToString( head (snd(snd(s))))
  print a
