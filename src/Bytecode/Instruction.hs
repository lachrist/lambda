module Bytecode.Instruction where

import Data.Map as M (Map, fromList)
import Data.Text (Text, pack)
import Data.Vector as V (Vector, fromList)
import Expression (Variable)
import Primitive (Primitive)

newtype Label = Label Text deriving (Eq, Ord, Show)

type Program = Map Label Block

data Block = Block {parameters :: [Variable], body :: Vector Instruction}

data Instruction
  = PrimitiveInstruction {literal :: Primitive}
  | LambdaInstruction {self :: Maybe Variable, definition :: Label}
  | ReadInstruction {variable :: Variable}
  | BranchInstruction {consequent :: Label, alternate :: Label}
  | LetInstruction {body :: Label}
  | ApplyInstruction {arity :: Int}

data TextualBlock = TextualBlock {label :: String, parameters :: [Variable], body :: [Instruction]}

type TextualProgram = [TextualBlock]

loadProgram :: TextualProgram -> Program
loadProgram = M.fromList . map loadBlock

loadBlock :: TextualBlock -> (Label, Block)
loadBlock (TextualBlock label params body) = (Label $ pack label, Block params (V.fromList body))
