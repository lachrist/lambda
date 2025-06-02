module Primitive where

data Primitive
  = NullPrimitive
  | NumberPrimitive Float
  | BooleanPrimitive Bool
  deriving (Eq, Show)
