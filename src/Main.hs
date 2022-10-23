module Main where

import Ast
import Data.Typeable
import Lexer
import Parser
import Token
import Utils
import ParserUtils
main = do
  let tokens = (parseTokens ("add((5));", []))
  -- print tokens
  -- let a = (PAR,([Token {typ = SEMICOLON, literal = ";"},Token {typ = EOF, literal = "EOF"}],[Statement {statementType = CALLSTA, statementUni = CallStatement, expression = CallExpression {expressionType = CALLEXP, callParams = [GroupedExpression {expressionType = GROUPEDEXP, groupedExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = "5"}, closed = False}], callIdent = IdentExpression {expressionType = IDENTEXP, ident = "add"}}}]))
  -- let p = paramHasOpen(PAR, snd(snd(a)))
  -- print p
  -- let l = getLastExpressionType(PAR, snd(snd a))
  -- print l
  -- let b = getLastNestedExpression(PAR, snd(snd a)) 
  -- print b
  -- let n = closeLastGrouped(PAR, snd(snd a))
  -- print n
  let s = parseStatements (EXP, (snd tokens, []))
  print s 

  let a = statementToString( head (snd(snd(s))))
  print a
