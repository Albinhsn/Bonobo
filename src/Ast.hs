module Ast where

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
  | STRINGEXP
  | ARRAYEXP 
  | INDEXEXP
  deriving (Eq, Show)

data Expression
  = OperatorExpression {expLine :: !Int, expressionType :: !ExpressionType, leftOperator :: !Expression, operator :: !Token, rightOperator :: !Expression}
  | IntegerLiteralExpression {expLine :: !Int, expressionType :: !ExpressionType, integerLiteral :: !Token}
  | ArrayExpression {expLine :: !Int, expressionType :: !ExpressionType, array :: ![Expression]}
  | IndexExpression {expLine :: !Int, expressionType :: !ExpressionType, arrayIdent :: !Token, arrayIndex :: !Int}
  | StringExpression {expLine :: !Int, expressionType :: !ExpressionType, stringLiteral :: !Token}
  | GroupedExpression {expLine :: !Int, expressionType :: !ExpressionType, groupedExpression :: !Expression, closed :: !Bool}
  | PrefixExpression {expLine :: !Int, expressionType :: !ExpressionType, prefixOperator :: !Token, prefixExpression :: !Expression}
  | BoolExpression {expLine :: !Int, expressionType :: !ExpressionType, leftBool :: !Expression, boolOperator :: !Token, rightBool :: !Expression}
  | TFExpression {expLine :: !Int, expressionType :: !ExpressionType, bool :: !TokenType}
  | Expression {expLine :: !Int, expressionType :: !ExpressionType}
  | IdentExpression {expLine :: !Int, expressionType :: !ExpressionType, ident :: !Token}
  | AssignExpression {expLine :: !Int, expressionType :: !ExpressionType, assignIdent :: !Expression, assignExpression :: !Expression}
  | CallExpression {expLine :: !Int, expressionType :: !ExpressionType, callParams :: ![Expression], callIdent :: !Expression, closedCall :: !Bool}
  deriving (Eq, Show)



data Statement = Statement
  { staLine :: !Int, 
    statementType :: !StatementType,
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
