module Ast where

import Data.Map (Map ())
import qualified Data.Map.Strict as Map
import Token

precedenceMap :: [(TokenType, Int)]
precedenceMap =
  [ (EQUALS, 1),
    (GREATER_T, 2),
    (LESS_T, 2),
    (MINUS, 3),
    (PLUS, 3),
    (ASTERISK, 4),
    (SLASH, 4),
    (PREFIX, 5),
    (INFIX, 5),
    (CALL, 6)
  ]

getPrecedence :: TokenType -> Int
getPrecedence t = head [snd n | n <- precedenceMap, fst n == t]

hasPrecedence :: (TokenType, TokenType) -> Bool
hasPrecedence (t1, t2) = getPrecedence t1 > getPrecedence t2

data Expression
  = OperatorExpression {leftOperator :: !Expression, operator :: !Token, rightOperator :: !Expression}
  | IntegerLiteralExpression {integerLiteral :: !String}
  | GroupedExpression {literalGrouped :: !Token}
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
