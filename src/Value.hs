module Value where

import Data.Map (Map)
import Expression
import Primitive (Primitive, PrimitiveBuiltin)

data Builtin
  = PrimitiveBuiltin PrimitiveBuiltin
  | ReferenceBuiltin ReferenceBuiltin
  deriving (Eq, Show)

data ReferenceBuiltin
  = Equal
  | Cons
  | Car
  | Cdr
  | SetCar
  | SetCdr
  deriving (Eq, Show)

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
