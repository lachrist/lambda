{-# LANGUAGE OverloadedStrings #-}

module Bytecode.Formatter (format) where

import Bytecode.Instruction (Block (..), Instruction (..), Label (..), Program (..))
import Data.Map as M (toAscList)
import Data.Text as T (Text, intercalate, pack, unlines)
import qualified Data.Vector as V
import Expression (Variable (..))
import Primitive (Primitive (..))

formatPrimitive :: Primitive -> Text
formatPrimitive NullPrimitive = "#n"
formatPrimitive (BooleanPrimitive True) = "#t"
formatPrimitive (BooleanPrimitive False) = "#f"
formatPrimitive (NumberPrimitive num) = pack $ show num

formatLabel :: Label -> Text
formatLabel (Label hash) = pack (show hash)

formatVariable :: Variable -> Text
formatVariable (Variable inner) = inner

formatProgram :: Program -> [T.Text]
formatProgram (Program (Label entry) body) =
  pack (show entry) : concatMap formatBlock (M.toAscList body)

formatBlock :: (Label, Block) -> [T.Text]
formatBlock (label, Block params body) =
  (formatLabel label <> " (" <> T.intercalate " " (map formatVariable params) <> "):")
    : map formatInstruction (V.toList body)

formatInstruction :: Instruction -> T.Text
formatInstruction (PushPrimitiveInstruction prim) =
  "  primitive " <> formatPrimitive prim
formatInstruction (PushLambdaInstruction Nothing label) =
  "  lambda " <> formatLabel label
formatInstruction (PushLambdaInstruction (Just self) label) =
  "  lambda " <> formatVariable self <> formatLabel label
formatInstruction (ReadInstruction var) =
  "  read " <> formatVariable var
formatInstruction (BranchInstruction label1 label2) =
  "  if " <> formatLabel label1 <> " " <> formatLabel label2
formatInstruction (LetGotoInstruction label) =
  "  let " <> formatLabel label
formatInstruction (ApplyInstruction arity) =
  "  apply " <> pack (show arity)

format :: Program -> Text
format = T.unlines . formatProgram
