module Bytecode.Lambda where

import Bytecode.Instruction (Label)
import Data.Map (Map)
import Expression (Variable)
import Value (Value)

data Lambda = Lambda
  { control :: Label,
    environment :: Map Variable Value
  }
  deriving (Show)
