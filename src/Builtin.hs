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
  | -- Type Assertion
    IsNull
  | IsBoolean
  | IsNumber
  | IsPair
  | IsProcedure
  | -- Boolean Algebra
    And
  | Or
  | Not
  | -- Number Comparison
    Equal
  | Greater
  | GreaterEqual
  | Lesser
  | LesserEqual
  | -- Number Algebra
    Plus
  | Minus
  | Mult
  | Div
  deriving (Eq, Show, Enum)

enumBuiltin :: [Builtin]
enumBuiltin = [Eq .. Div]

getBuiltinName :: Builtin -> Text
-- Reference
getBuiltinName Eq = "eq?"
getBuiltinName Cons = "cons"
getBuiltinName Car = "car"
getBuiltinName Cdr = "cdr"
getBuiltinName SetCar = "set-car!"
getBuiltinName SetCdr = "set-cdr!"
getBuiltinName Begin = "begin"
-- Type Assertion
getBuiltinName IsNull = "null?"
getBuiltinName IsBoolean = "boolean?"
getBuiltinName IsNumber = "number?"
getBuiltinName IsPair = "pair?"
getBuiltinName IsProcedure = "procedure?"
-- Boolean Algebra
getBuiltinName And = "and"
getBuiltinName Or = "or"
getBuiltinName Not = "not"
-- Number Comparison
getBuiltinName Equal = "="
getBuiltinName Greater = ">"
getBuiltinName GreaterEqual = ">="
getBuiltinName Lesser = "<"
getBuiltinName LesserEqual = "<="
-- Number Algebra
getBuiltinName Plus = "+"
getBuiltinName Minus = "-"
getBuiltinName Mult = "*"
getBuiltinName Div = "/"
