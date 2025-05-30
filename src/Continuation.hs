module Continuation where

import Data.Map (Map)
import Expression (Expression, Variable)
import Value (Value)

data Continuation
  = IfContinuation
      { environment :: Map Variable Value,
        consequent :: Expression,
        alternate :: Expression
      }
  | LetContinuation
      { environment :: Map Variable Value,
        bound :: Variable,
        body :: Expression
      }
  | ApplyContinuation
      { environment :: Map Variable Value,
        todo :: [Expression],
        done :: [Value]
      }
