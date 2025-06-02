{-# LANGUAGE OverloadedStrings #-}

module Bytecode.Formatter (format) where

import Bytecode.Instruction (Block (..), Instruction (..), Label (..), Program (..))
import Data.Map as M (toAscList)
import Data.Text as T (Text, intercalate, pack, unlines)
import qualified Data.Vector as V
import Expression (Variable (..))
import Primitive (formatPrimitive)

formatLabel :: Label -> Text
formatLabel (Label hash) = pack (show hash)

formatVariable :: Variable -> Text
formatVariable (Variable inner) = inner

formatMaybeVariable :: Maybe Variable -> Text
formatMaybeVariable (Just variable) = formatVariable variable
formatMaybeVariable Nothing = "."

formatProgram :: Program -> [T.Text]
formatProgram (Program (Label entry) body) =
  pack (show entry) : concatMap formatBlock (M.toAscList body)

formatBlock :: (Label, Block) -> [T.Text]
formatBlock (label, Block params body) =
  (formatLabel label <> " (" <> T.intercalate " " (map formatVariable params) <> "):")
    : map formatInstruction (V.toList body)

formatInstruction :: Instruction -> T.Text
formatInstruction (PrimitiveInstruction prim) =
  "  primitive " <> pack (formatPrimitive prim)
formatInstruction (LambdaInstruction self label) =
  "  lambda " <> formatMaybeVariable self <> " " <> formatLabel label
formatInstruction (ReadInstruction var) =
  "  read " <> formatVariable var
formatInstruction (IfInstruction label1 label2) =
  "  if " <> formatLabel label1 <> " " <> formatLabel label2
formatInstruction (LetInstruction label) =
  "  let " <> formatLabel label
formatInstruction (ApplyInstruction arity) =
  "  apply " <> pack (show arity)

format :: Program -> Text
format = T.unlines . formatProgram
