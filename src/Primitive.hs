{-# LANGUAGE DeriveGeneric #-}

module Primitive where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Primitive
  = NullPrimitive
  | NumberPrimitive Float
  | BooleanPrimitive Bool
  deriving (Eq, Show, Generic)

formatPrimitive :: Primitive -> String
formatPrimitive NullPrimitive = "#n"
formatPrimitive (BooleanPrimitive True) = "#t"
formatPrimitive (BooleanPrimitive False) = "#f"
formatPrimitive (NumberPrimitive num) = show num

instance Hashable Primitive