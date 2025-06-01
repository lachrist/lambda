module Eval.Value where

import Builtin (Builtin)
import Data.Map (Map)
import Expression (Expression, Variable)
import Primitive (Primitive)

type Address = Int

data Value
  = BuiltinImmediate Builtin
  | PrimitiveImmediate Primitive
  | Reference Address
  deriving (Eq, Show)

data IndirectValue
  = PairIndirect {car :: Value, cdr :: Value}
  | LambdaIndirect
      { environment :: Map Variable Value,
        parameters :: [Variable],
        body :: Expression
      }
  deriving (Show)
