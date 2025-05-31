module Expression where

import Data.Text (Text)
import Primitive (Primitive)

type Variable = Text

data Expression
  = PrimitiveExpression {inner :: Primitive}
  | VariableExpression {read :: Variable}
  | LambdaExpression {parameters :: [Variable], body :: Expression}
  | LetExpression {left :: Variable, right :: Expression, body :: Expression}
  | IfExpression {test :: Expression, consequent :: Expression, alternate :: Expression}
  | ApplicationExpression {callee :: Expression, arguments :: [Expression]}
  deriving (Show)
