module Primitive where

import Builtin (Builtin (And, Div, Equal, Greater, GreaterEqual, Minus, Mult, Not, Or, Plus))
import Control.Monad.Except (ExceptT, MonadError (throwError))

data Primitive
  = NullPrimitive
  | NumberPrimitive Float
  | BooleanPrimitive Bool
  deriving (Eq, Show)

applyPrimitiveBuiltin :: (Monad m) => Builtin -> [Primitive] -> ExceptT String m Primitive
applyPrimitiveBuiltin Equal [NumberPrimitive x, NumberPrimitive y] = return $ BooleanPrimitive $ x == y
applyPrimitiveBuiltin Greater [NumberPrimitive x, NumberPrimitive y] = return $ BooleanPrimitive $ x < y
applyPrimitiveBuiltin GreaterEqual [NumberPrimitive x, NumberPrimitive y] = return $ BooleanPrimitive $ x <= y
applyPrimitiveBuiltin Plus [NumberPrimitive x, NumberPrimitive y] = return $ NumberPrimitive $ x + y
applyPrimitiveBuiltin Minus [NumberPrimitive x, NumberPrimitive y] = return $ NumberPrimitive $ x - y
applyPrimitiveBuiltin Mult [NumberPrimitive x, NumberPrimitive y] = return $ NumberPrimitive $ x * y
applyPrimitiveBuiltin Div [NumberPrimitive x, NumberPrimitive y] = return $ NumberPrimitive $ x / y
applyPrimitiveBuiltin And [BooleanPrimitive x, BooleanPrimitive y] = return $ BooleanPrimitive $ x && y
applyPrimitiveBuiltin Or [BooleanPrimitive x, BooleanPrimitive y] = return $ BooleanPrimitive $ x || y
applyPrimitiveBuiltin Not [BooleanPrimitive x] = return $ BooleanPrimitive $ not x
applyPrimitiveBuiltin builtin arguments = throwError $ show builtin ++ " >> " ++ show arguments
