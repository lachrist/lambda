module Environment (Environment, initialEnvironment) where

import Builtin (Builtin, enumBuiltin, getBuiltinName)
import Data.Map (Map, fromList)
import Expression (Variable (Variable))
import Value (Value (BuiltinImmediate))

type Environment = Map Variable Value

toBuiltinBinding :: Builtin -> (Variable, Value)
toBuiltinBinding builtin =
  (Variable $ getBuiltinName builtin, BuiltinImmediate builtin)

initialEnvironment :: Environment
initialEnvironment =
  fromList
    (map toBuiltinBinding enumBuiltin)
