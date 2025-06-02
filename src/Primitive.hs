{-# LANGUAGE DeriveGeneric #-}

module Primitive where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data Primitive
  = NullPrimitive
  | NumberPrimitive Float
  | BooleanPrimitive Bool
  deriving (Eq, Show, Generic)

instance Hashable Primitive