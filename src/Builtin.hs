module Builtin where

data Builtin
  = -- Reference
    Eq
  | Cons
  | Car
  | Cdr
  | SetCar
  | SetCdr
  | --  Comparison
    Equal
  | Greater
  | GreaterEqual
  | -- Boolean
    And
  | Or
  | Not
  | -- Arithmetic
    Plus
  | Minus
  | Mult
  | Div
  deriving (Eq, Show)
