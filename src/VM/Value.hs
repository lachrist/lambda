module VM.Value where

import Builtin (Builtin)
import Data.Map (Map)
import Expression (Variable)
import Primitive (Primitive)
import VM.Bytecode (Label)

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
        label :: Label
      }
  deriving (Show)
