{-# LANGUAGE OverloadedStrings #-}

module Bytecode.Parser where

import Bytecode.Instruction (Block (..), Instruction (..), Label (..), Program (..))
import Control.Applicative (liftA2, (<|>))
import Control.Monad (void)
import Data.Functor (($>), (<&>))
import qualified Data.Map as M
import qualified Data.Vector as V
import Expression (Variable)
import File (File (..))
import Parser
  ( Parser,
    lexeme,
    parsePrimitive,
    parseVariable,
    space,
    symbol,
  )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

parseReadInstruction :: Parser Instruction
parseReadInstruction =
  symbol "read" >> parseVariable <&> ReadInstruction

parsePrimitiveInstruction :: Parser Instruction
parsePrimitiveInstruction =
  symbol "primitive" >> parsePrimitive <&> PrimitiveInstruction

parseMaybeVariable :: Parser (Maybe Variable)
parseMaybeVariable =
  (symbol "." $> Nothing) <|> (parseVariable <&> Just)

parseLabel :: Parser Label
parseLabel =
  lexeme L.decimal <&> Label

parseArity :: Parser Int
parseArity =
  lexeme L.decimal

parseLambdaInstruction :: Parser Instruction
parseLambdaInstruction =
  symbol "lambda" >> liftA2 LambdaInstruction parseMaybeVariable parseLabel

parseLetInstruction :: Parser Instruction
parseLetInstruction =
  symbol "let" >> fmap LetInstruction parseLabel

parseIfInstruction :: Parser Instruction
parseIfInstruction =
  symbol "if" >> liftA2 IfInstruction parseLabel parseLabel

parseApplyInstruction :: Parser Instruction
parseApplyInstruction =
  symbol "apply" >> fmap ApplyInstruction parseArity

parseInstruction :: Parser Instruction
parseInstruction =
  P.choice
    [ parsePrimitiveInstruction,
      parseReadInstruction,
      parseLambdaInstruction,
      parseLetInstruction,
      parseIfInstruction,
      parseApplyInstruction
    ]

parseBlock :: Parser (Label, Block)
parseBlock = do
  label <- parseLabel
  void $ symbol "("
  params <- P.many parseVariable
  void $ symbol ")"
  void $ symbol ":"
  body <- P.many parseInstruction
  return (label, Block params (V.fromList body))

parseProgram :: Parser Program
parseProgram = do
  main <- parseLabel
  bindings <- P.many parseBlock
  return $ Program main (M.fromList bindings)

parseFile :: File -> Either String Program
parseFile (File path content) =
  case P.runParser (space *> parseProgram <* P.eof) path content of
    Left failure -> Left $ P.errorBundlePretty failure
    Right expression -> Right expression
