module Ast where

import Data.Map (Map ())
import qualified Data.Map.Strict as Map
import Token

data PrecedenceType = EQUAL | LESSGREATER | SUM | PRODUCT | PREFIX | CALL | LOWEST deriving (Eq, Show)

precedenceMap :: [(PrecedenceType, Int)]
precedenceMap =
  [ (LOWEST, 0),
    (EQUAL, 2),
    (LESSGREATER, 1),
    (SUM, 3),
    (PRODUCT, 4),
    (PREFIX, 5),
    (CALL, 6)
  ]

getPrecedenceTypeFromTokenType :: TokenType -> PrecedenceType
getPrecedenceTypeFromTokenType t = p
  where
    p
      | t == PLUS || t == MINUS = SUM
      | t == BANG = PREFIX
      | t == EQUALS = EQUAL
      | t == ASTERISK || t == SLASH = PRODUCT
      | t == GREATER_T || t == LESS_T = LESSGREATER
      | t == FUNCTION = CALL
      | otherwise = error "can't map tokentype to precedence"

getPrecedence :: TokenType -> Int
getPrecedence t = head [snd n | n <- precedenceMap, fst n == getPrecedenceTypeFromTokenType t]

hasPrecedence :: (TokenType, TokenType) -> Bool
hasPrecedence (t1, t2) = getPrecedence t1 > getPrecedence t2

data ExpressionType
  = OPERATOREXP
  | BOOLEXP
  | INTEXP
  | GROUPEDEXP
  | PREFIXEXP
  | EMPTYEXP
  | TFEXP
  | IDENTEXP
  | FUNCEXP 
  | ASSIGNEXP
  | CALLEXP
  deriving (Eq, Show)

data Expression
  = OperatorExpression {expressionType :: !ExpressionType, leftOperator :: !Expression, operator :: !Token, rightOperator :: !Expression}
  | IntegerLiteralExpression {expressionType :: !ExpressionType, integerLiteral :: !Token}
  | GroupedExpression {expressionType :: !ExpressionType, groupedExpression :: !Expression, closed :: !Bool}
  | PrefixExpression {expressionType :: !ExpressionType, prefixOperator :: !Token, prefixExpression :: !Expression}
  | BoolExpression {expressionType :: !ExpressionType, leftBool :: !Expression, boolOperator :: !Token, rightBool :: !Expression}
  | TFExpression {expressionType :: !ExpressionType, bool :: !TokenType}
  | Expression {expressionType :: !ExpressionType}
  | IdentExpression {expressionType :: !ExpressionType, ident :: !Token}
  | AssignExpression {expressionType :: !ExpressionType, assignIdent :: !Expression, assignExpression :: !Expression}
  | CallExpression {expressionType :: !ExpressionType, callParams :: ![Expression], callIdent :: !Expression, closedCall :: !Bool}
  deriving (Eq, Show)

data Statement = Statement
  { statementType :: !StatementType,
    statementUni :: !StatementUni,
    expression :: !Expression
  }
  deriving
    ( Eq,
      Show
    )
data BlockType = CON | ALT | EXP | PAR | BOD deriving(Eq, Show) 

data StatementType = 
  NOSTA 
  | LETSTA 
  | RETSTA 
  | IFSTA 
  | FUNCSTA 
  | ASSIGNSTA 
  | CALLSTA
  deriving (Eq, Show)

data StatementUni
  = LetStatement {identifier :: !String}
  | ReturnStatement {}
  | IfStatement {closedCon :: !Bool, con :: ![Statement], alt :: ![Statement], closedAlt :: !Bool}
  | FuncStatement {params :: ![Expression], body :: ![Statement]} 
  | CallStatement {} 
  | AssignStatement{}
  | NoStatement {}
  deriving
    ( Eq,
      Show
    )
