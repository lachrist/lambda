{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (void)
import Data.Char (isAlpha)
import Data.Text (Text)
import Data.Void (Void)
import Expression (Expression (ApplicationExpression, IfExpression, LetExpression, PrimitiveExpression, VariableExpression))
import Primitive (Primitive (BooleanPrimitive, NullPrimitive, NumberPrimitive))
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
parseNumber = lexeme L.float

parseBoolean :: Parser Bool
parseBoolean = (True <$ symbol "#t") <|> (False <$ symbol "#f")

parseNull :: Parser ()
parseNull = void $ symbol "#n"

isVariableChar :: Char -> Bool
isVariableChar char = isAlpha char || char `elem` ("+-*/?!" :: String)

parseVariable :: Parser Text
parseVariable = lexeme $ P.takeWhile1P (Just "variable") isVariableChar

parseIfExpression :: Parser Expression
parseIfExpression = do
  void $ symbol "("
  void $ symbol "if"
  test <- parseExpression
  consequent <- parseExpression
  alternate <- parseExpression
  void $ symbol ")"
  return $ IfExpression test consequent alternate

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
      parseIfExpression,
      parseLetExpression,
      parseApplicationExpression
    ]

data File = File {path :: String, content :: Text}

parseFile :: File -> Either String Expression
parseFile (File {path, content}) = case P.runParser parseExpression path content of
  Left failure -> Left $ show failure
  Right expression -> Right expression
