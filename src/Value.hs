module Value (Value (..), IndirectValue (..), Address, toBool, applyBuiltin) where

import Builtin (Builtin (..))
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.ST (ST)
import Data.Maybe (fromMaybe)
import Memory (Memory (..))
import Primitive (Primitive (..))

type Address = Int

data Value
  = BuiltinImmediate Builtin
  | PrimitiveImmediate Primitive
  | Reference Address
  deriving (Eq, Show)

data IndirectValue l
  = PairIndirect {car :: Value, cdr :: Value}
  | LambdaIndirect {lambda :: l}
  deriving (Show)

------------
-- Helper --
------------

toBool :: (Monad m) => Value -> ExceptT String m Bool
toBool (PrimitiveImmediate (BooleanPrimitive bool)) = return bool
toBool val = throwError $ "Expected a boolean, but gor: " ++ show val

isLambda :: IndirectValue a -> Bool
isLambda (LambdaIndirect {}) = True
isLambda _ = False

isPair :: IndirectValue a -> Bool
isPair (PairIndirect {}) = True
isPair _ = False

toPair :: (Monad m, Show l) => IndirectValue l -> ExceptT String m (Value, Value)
toPair (PairIndirect x y) = return (x, y)
toPair indirect = throwError $ "Expected a pair but got: " ++ show indirect

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs

-------------
-- Builtin --
-------------

applyBuiltin :: (Memory m, Show l) => Builtin -> [Value] -> m x Address (IndirectValue l) -> ExceptT String (ST x) Value
-- Begin --
applyBuiltin Begin vals _ =
  return $ fromMaybe (PrimitiveImmediate NullPrimitive) (lastMaybe vals)
-- Eq --
applyBuiltin Eq [val1, val2] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ val1 == val2
-- Cons --
applyBuiltin Cons [val1, val2] mem = do
  addr <- Memory.init mem
  save addr (PairIndirect val1 val2) mem
  return $ Reference addr
-- Car --
applyBuiltin Car [Reference addr] store =
  fmap fst (load addr store >>= toPair)
-- Cdr --
applyBuiltin Cdr [Reference addr] store =
  fmap snd (load addr store >>= toPair)
-- SetCar --
applyBuiltin SetCar [Reference addr, val1] store = do
  (_, val2) <- load addr store >>= toPair
  save addr (PairIndirect val1 val2) store
  return $ PrimitiveImmediate NullPrimitive
-- SetCdr --
applyBuiltin SetCdr [Reference addr, val2] store = do
  (val1, _) <- load addr store >>= toPair
  save addr (PairIndirect val1 val2) store
  return $ PrimitiveImmediate NullPrimitive
-- IsNull --
applyBuiltin IsNull [PrimitiveImmediate NullPrimitive] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsNull _ _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
-- IsBoolean --
applyBuiltin IsBoolean [PrimitiveImmediate (BooleanPrimitive _)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsBoolean _ _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
-- IsNumber --
applyBuiltin IsNumber [PrimitiveImmediate (NumberPrimitive _)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsNumber _ _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
-- IsPair --
applyBuiltin IsPair [BuiltinImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
applyBuiltin IsPair [PrimitiveImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
applyBuiltin IsPair [Reference addr] store =
  PrimitiveImmediate . BooleanPrimitive . isPair <$> load addr store
-- IsProcedure --
applyBuiltin IsProcedure [BuiltinImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsProcedure [PrimitiveImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
applyBuiltin IsProcedure [Reference addr] store =
  PrimitiveImmediate . BooleanPrimitive . isLambda <$> load addr store
-- And --
applyBuiltin And [PrimitiveImmediate (BooleanPrimitive x), PrimitiveImmediate (BooleanPrimitive y)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ x && y
-- Or --
applyBuiltin Or [PrimitiveImmediate (BooleanPrimitive x), PrimitiveImmediate (BooleanPrimitive y)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ x || y
-- Not --
applyBuiltin Not [PrimitiveImmediate (BooleanPrimitive x)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ not x
-- Equal --
applyBuiltin Equal [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ x == y
-- Greater --
applyBuiltin Greater [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ x > y
-- GreaterEqual --
applyBuiltin GreaterEqual [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ x >= y
-- Lesser --
applyBuiltin Lesser [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ x > y
-- LesserEqual --
applyBuiltin LesserEqual [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ x >= y
-- Plus --
applyBuiltin Plus [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ NumberPrimitive $ x + y
-- Minus --
applyBuiltin Minus [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ NumberPrimitive $ x - y
-- Mult --
applyBuiltin Mult [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ NumberPrimitive $ x * y
-- Div --
applyBuiltin Div [PrimitiveImmediate (NumberPrimitive x), PrimitiveImmediate (NumberPrimitive y)] _ =
  return $ PrimitiveImmediate $ NumberPrimitive $ x / y
-- Type Error --
applyBuiltin builtin arguments _ =
  throwError $ "Cannot apply " ++ show builtin ++ " onto " ++ show arguments