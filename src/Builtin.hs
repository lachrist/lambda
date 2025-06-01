{-# LANGUAGE OverloadedStrings #-}

module Builtin where

import Data.Text (Text)

data Builtin
  = -- Reference
    Eq
  | Cons
  | Car
  | Cdr
  | SetCar
  | SetCdr
  | Begin
  | --  Comparison
    Equal
  | Greater
  | GreaterEqual
  | Lesser
  | LesserEqual
  | -- Boolean
    And
  | Or
  | Not
  | -- Arithmetic
    Plus
  | Minus
  | Mult
  | Div
  deriving (Eq, Show, Enum)

enumBuiltin :: [Builtin]
enumBuiltin = [Eq .. Div]

getBuiltinName :: Builtin -> Text
getBuiltinName Eq = "eq?"
getBuiltinName Cons = "cons"
getBuiltinName Car = "car"
getBuiltinName Cdr = "cdr"
getBuiltinName SetCar = "set-car!"
getBuiltinName SetCdr = "set-cdr!"
getBuiltinName Begin = "begin"
getBuiltinName Equal = "="
getBuiltinName Greater = ">"
getBuiltinName GreaterEqual = ">="
getBuiltinName Lesser = "<"
getBuiltinName LesserEqual = "<="
getBuiltinName And = "and"
getBuiltinName Or = "or"
getBuiltinName Not = "not"
getBuiltinName Plus = "+"
getBuiltinName Minus = "-"
getBuiltinName Mult = "*"
getBuiltinName Div = "/"
