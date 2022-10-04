module Ast where

import Token

data NodeType = STATEMENT | EXPRESSION deriving (Eq, Show)

data ReturnStatement = ReturnStatement {} deriving (Eq, Show)

data Identifier = Identifier {identifierToken :: Token, identifierValue :: !String} deriving (Eq, Show)

--  let x = 5;
-- LetStatement
--   {
--    identifier = x,
--    expression = IntegerLiteralExpression{integerLiteral = 5}
--    }

-- let x = 5 + 5;
-- LetStatement
-- {
--   identifier = x,
--   expression = OperatorExpression{
--     leftOperator = IntegerLiteralExpression {integerLiteral= 5},
--     operator = PLUS,
--     rightOperator = IntegerLiteralExpression {integerLiteral=5}
--   }
-- }

-- let x = 5 + 5 + 5;

-- let foobar = add(5, 5);
-- LetStatement
-- {
--     identifier = "foobar",
--     expression =
--   }

-- let barfoo = 5 * 5 / 10 + 18 - add(5, 5) + multiply(124);
-- let anotherName = barfoo;

data Expression
  = OperatorExpression {leftOperator :: Expression, operator :: Token, rightOperator :: Expression}
  | IntegerLiteralExpression {integerLiteral :: Int}
  | GroupedExpression {literalGrouped :: Token}
  | Expression {expr :: String}
  deriving (Eq, Show)

data Statement = LetStatement {identifier :: String, expression :: Expression}
  deriving
    ( -- | ReturnSatement {}
      -- | IfStatement {}
      Eq,
      Show
    )
