module Primitive where

import Control.Monad.Except (ExceptT, MonadError (throwError))

data Primitive
  = Null
  | PrimitiveNumber Float
  | PrimitiveBoolean Bool
  deriving (Eq, Show)

data PrimitiveBuiltin
  = -- Comparison
    Greater
  | GreaterEqual
  | -- Boolean
    And
  | Or
  | Not
  | -- Arithmetic
    Plus
  | Minus
  | Mult
  | Div
  deriving (Eq, Show)

applyPrimitiveBuiltin :: (Monad m) => PrimitiveBuiltin -> [Primitive] -> ExceptT String m Primitive
applyPrimitiveBuiltin Greater [PrimitiveNumber x, PrimitiveNumber y] = return $ PrimitiveBoolean $ x < y
applyPrimitiveBuiltin GreaterEqual [PrimitiveNumber x, PrimitiveNumber y] = return $ PrimitiveBoolean $ x <= y
applyPrimitiveBuiltin Plus [PrimitiveNumber x, PrimitiveNumber y] = return $ PrimitiveNumber $ x + y
applyPrimitiveBuiltin Minus [PrimitiveNumber x, PrimitiveNumber y] = return $ PrimitiveNumber $ x - y
applyPrimitiveBuiltin Mult [PrimitiveNumber x, PrimitiveNumber y] = return $ PrimitiveNumber $ x * y
applyPrimitiveBuiltin Div [PrimitiveNumber x, PrimitiveNumber y] = return $ PrimitiveNumber $ x / y
applyPrimitiveBuiltin And [PrimitiveBoolean x, PrimitiveBoolean y] = return $ PrimitiveBoolean $ x && y
applyPrimitiveBuiltin Or [PrimitiveBoolean x, PrimitiveBoolean y] = return $ PrimitiveBoolean $ x || y
applyPrimitiveBuiltin Not [PrimitiveBoolean x] = return $ PrimitiveBoolean $ not x
applyPrimitiveBuiltin builtin arguments = throwError $ show builtin ++ " >> " ++ show arguments
