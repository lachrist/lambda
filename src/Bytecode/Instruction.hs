module Bytecode.Instruction where

import Data.Map as M (Map)
import Data.Vector as V (Vector)
import Expression (Variable)
import Primitive (Primitive)

newtype Label = Label Word deriving (Eq, Ord, Show)

type ProgramBody = Map Label Block

data Program = Program
  { entry :: Label,
    body :: ProgramBody
  }
  deriving (Show)

data Block = Block
  { parameters :: [Variable],
    body :: Vector Instruction
  }
  deriving (Show)

data Instruction
  = PrimitiveInstruction {literal :: Primitive}
  | LambdaInstruction {self :: Maybe Variable, definition :: Label}
  | ReadInstruction {variable :: Variable}
  | IfInstruction {consequent :: Label, alternate :: Label}
  | LetInstruction {body :: Label}
  | ApplyInstruction {arity :: Int}
  deriving (Show)
