module Expression where

import Primitive (Primitive)

type Variable = String

data Expression
  = PrimitiveExpression {inner :: Primitive}
  | VariableExpression {read :: Variable}
  | LambdaExpression {parameters :: [Variable], body :: Expression}
  | LetExpression {left :: Variable, right :: Expression, body :: Expression}
  | IfExpression {test :: Expression, consequent :: Expression, alternate :: Expression}
  | ApplicationExpression {callee :: Expression, arguments :: [Expression]}
  deriving (Show)