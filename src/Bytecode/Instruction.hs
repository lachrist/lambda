module Bytecode.Instruction where

import Data.Map as M (Map)
import Data.Vector as V (Vector)
import Expression (Variable)
import Primitive (Primitive)

newtype Label = Label Int deriving (Eq, Ord, Show)

type ProgramBody = Map Label Block

data Program = Program
  { entry :: Label,
    body :: ProgramBody
  }

data Block = Block {parameters :: [Variable], body :: Vector Instruction}

data Instruction
  = PushPrimitiveInstruction {literal :: Primitive}
  | PushLambdaInstruction {self :: Maybe Variable, definition :: Label}
  | ReadInstruction {variable :: Variable}
  | BranchInstruction {consequent :: Label, alternate :: Label}
  | LetGotoInstruction {body :: Label}
  | ApplyInstruction {arity :: Int}
