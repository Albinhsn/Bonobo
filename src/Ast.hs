module Ast where

import Data.Map (Map ())
import qualified Data.Map.Strict as Map
import Token

data PrecedenceType = EQUAL | LESSGREATER | SUM | PRODUCT | PREFIX | CALL | LOWEST deriving (Eq, Show)

precedenceMap :: [(PrecedenceType, Int)]
precedenceMap =
  [ (LOWEST, 0),
    (EQUAL, 1),
    (LESSGREATER, 2),
    (SUM, 3),
    (PRODUCT, 4),
    (PREFIX, 5),
    (CALL, 6)
  ]

getPrecedenceTypeFromTokenType :: TokenType -> PrecedenceType
getPrecedenceTypeFromTokenType t = p
  where
    p
      | t == PLUS = SUM
      | t == MINUS || t == BANG = PREFIX
      | t == EQUALS = EQUAL
      | t == ASTERISK = PRODUCT
      | t == GREATER_T || t == LESS_T = LESSGREATER
      | t == FUNCTION = CALL
      | otherwise = error "can't map tokentype to precedence"

getPrecedence :: TokenType -> Int
getPrecedence t = head [snd n | n <- precedenceMap, fst n == getPrecedenceTypeFromTokenType t]

hasPrecedence :: (TokenType, TokenType) -> Bool
hasPrecedence (t1, t2) = getPrecedence t1 > getPrecedence t2

data Expression
  = OperatorExpression {leftOperator :: !Expression, operator :: !Token, rightOperator :: !Expression}
  | IntegerLiteralExpression {integerLiteral :: !String}
  | GroupedExpression {literalGrouped :: !Token}
  | InfixExpression {infixOperator :: !Token, infixExpression :: !Expression}
  | PrefixExpression {leftExpression :: !Expression, infixOperator :: !Token, rightExpression :: !Expression}
  | Expression {}
  deriving (Eq, Show)

data Statement = Statement
  { statementType :: !StatementType,
    expression :: !Expression
  }
  deriving
    ( Eq,
      Show
    )

data StatementType
  = LetStatement {identifier :: !String}
  | ReturnStatement {}
  | IfStatement {}
  deriving
    ( Eq,
      Show
    )
