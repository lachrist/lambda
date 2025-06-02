{-# LANGUAGE DeriveGeneric #-}

module Expression where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Primitive (Primitive)

newtype Variable = Variable Text deriving (Eq, Ord, Show, Generic)

data Expression
  = PrimitiveExpression {inner :: Primitive}
  | VariableExpression {read :: Variable}
  | LambdaExpression {parameters :: [Variable], body :: Expression}
  | LetExpression {left :: Variable, right :: Expression, body :: Expression}
  | IfExpression {test :: Expression, consequent :: Expression, alternate :: Expression}
  | ApplicationExpression {callee :: Expression, arguments :: [Expression]}
  deriving (Eq, Show, Generic)

instance Hashable Variable

instance Hashable Expression
