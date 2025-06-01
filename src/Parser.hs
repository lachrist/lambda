{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (parseFile, File (File)) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (void)
import Data.Char (isAlpha)
import Data.Text (Text)
import Data.Void (Void)
import Expression
  ( Expression
      ( ApplicationExpression,
        IfExpression,
        LambdaExpression,
        LetExpression,
        PrimitiveExpression,
        VariableExpression
      ),
    Variable (Variable),
  )
import Primitive (Primitive (BooleanPrimitive, NullPrimitive, NumberPrimitive))
import Text.Megaparsec (errorBundlePretty, try)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void Text

space :: Parser ()
space = L.space C.space1 (L.skipLineComment ";") P.empty

lexeme :: Parser a -> Parser a
lexeme p = p <* space

symbol :: Text -> Parser Text
symbol = L.symbol space

parseNumber :: Parser Float
parseNumber = lexeme (try L.float <|> L.decimal)

parseBoolean :: Parser Bool
parseBoolean = (True <$ symbol "#t") <|> (False <$ symbol "#f")

parseNull :: Parser ()
parseNull = void $ symbol "#n"

enumOperatorChar :: [Char]
enumOperatorChar = "+-*/?!=><"

isVariableChar :: Char -> Bool
isVariableChar char = isAlpha char || char `elem` enumOperatorChar

parseVariable :: Parser Variable
parseVariable = lexeme $ Variable <$> P.takeWhile1P (Just "variable") isVariableChar

parseIfExpression :: Parser Expression
parseIfExpression = do
  void $ symbol "("
  void $ symbol "if"
  test <- parseExpression
  consequent <- parseExpression
  alternate <- parseExpression
  void $ symbol ")"
  return $ IfExpression test consequent alternate

parseLambdaExpression :: Parser Expression
parseLambdaExpression = do
  void $ symbol "("
  void $ symbol "lambda"
  void $ symbol "("
  parameters <- many parseVariable
  void $ symbol ")"
  body <- parseExpression
  void $ symbol ")"
  return $ LambdaExpression parameters body

parseLetExpression :: Parser Expression
parseLetExpression = do
  void $ symbol "("
  void $ symbol "let"
  left <- parseVariable
  right <- parseExpression
  body <- parseExpression
  void $ symbol ")"
  return $ LetExpression left right body

parseApplicationExpression :: Parser Expression
parseApplicationExpression = do
  void $ symbol "("
  callee <- parseExpression
  arguments <- many parseExpression
  void $ symbol ")"
  return $ ApplicationExpression callee arguments

parseExpression :: Parser Expression
parseExpression =
  P.choice
    [ PrimitiveExpression NullPrimitive <$ parseNull,
      PrimitiveExpression . BooleanPrimitive <$> parseBoolean,
      PrimitiveExpression . NumberPrimitive <$> parseNumber,
      VariableExpression <$> parseVariable,
      try parseIfExpression,
      try parseLetExpression,
      try parseLambdaExpression,
      parseApplicationExpression
    ]

data File = File {path :: String, content :: Text}

parseFile :: File -> Either String Expression
parseFile (File {path, content}) =
  case P.runParser (space *> parseExpression <* P.eof) path content of
    Left failure -> Left $ errorBundlePretty failure
    Right expression -> Right expression
