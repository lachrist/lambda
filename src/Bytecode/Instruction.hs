module Bytecode.Instruction where

import Data.Map (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Expression (Variable)
import Primitive (Primitive)

newtype Label = Label Text deriving (Eq, Ord, Show)

type Program = Map Label Block

data Block = Block {parameters :: [Variable], body :: Vector Instruction}

type TextualProgram = [(Label, Block)]

data Instruction
  = PrimitiveInstruction {literal :: Primitive}
  | LambdaInstruction {self :: Maybe Variable, definition :: Label}
  | ReadInstruction {variable :: Variable}
  | BranchInstruction {consequent :: Label, alternate :: Label}
  | LetInstruction {body :: Label}
  | ApplyInstruction {arity :: Int}
