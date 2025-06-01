{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import Builtin
  ( Builtin (Begin, Car, Cdr, Cons, Eq, IsBoolean, IsNull, IsNumber, IsPair, IsProcedure, SetCar, SetCdr),
    enumBuiltin,
    getBuiltinName,
  )
import Continuation (Continuation (ApplyContinuation, IfContinuation, LetContinuation))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Except (ExceptT)
import Data.Array.MArray (newArray)
import Data.Map (Map, fromList, insert, lookup, union)
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
import Primitive (Primitive (BooleanPrimitive, NullPrimitive, NumberPrimitive), applyPrimitiveBuiltin)
import Store (Memory (HoleArray), Store (init, load, save))
import Value
  ( Address,
    IndirectValue (LambdaIndirect, PairIndirect),
    Value (BuiltinImmediate, PrimitiveImmediate, Reference),
  )

data State x s
  = Ongoing
      { control :: Expression,
        env :: Map Variable Value,
        store :: s x Address IndirectValue,
        cont :: [Continuation]
      }
  | Success
      { completion :: Value,
        store :: s x Address IndirectValue
      }

------------
-- Helper --
------------

branch :: (Monad m) => Value -> ExceptT String m Bool
branch (PrimitiveImmediate (BooleanPrimitive bool)) = return bool
branch _ = throwError "not a boolean"

unwrapPrimitive :: (Monad m) => Value -> ExceptT String m Primitive
unwrapPrimitive (PrimitiveImmediate primitive) = return primitive
unwrapPrimitive value = throwError $ "expected a primitive >> " ++ show value

lastExcept :: (Monad m) => [a] -> ExceptT String m a
lastExcept [] = throwError "cannt access the last element of an empty list"
lastExcept vs = return $ last vs

isLambda :: IndirectValue -> Bool
isLambda (LambdaIndirect {}) = True
isLambda _ = False

isPair :: IndirectValue -> Bool
isPair (PairIndirect {}) = True
isPair _ = False

----------
-- Step --
----------

step :: (Store s) => State x s -> ExceptT String (ST x) (State x s)
step state@(Success {}) = return state
step (Ongoing (PrimitiveExpression inner) _ store cont) =
  continue (PrimitiveImmediate inner) cont store
step (Ongoing (VariableExpression variable) env store cont) =
  case Data.Map.lookup variable env of
    Nothing -> throwError $ "missing variable :" ++ show variable
    Just value -> continue value cont store
step (Ongoing (LambdaExpression params body) env store cont) =
  do
    addr <- Store.init store
    save addr (LambdaIndirect env params body) store
    continue (Reference addr) cont store
step (Ongoing (IfExpression test consequent alternate) env store cont) =
  return $ Ongoing test env store (IfContinuation env consequent alternate : cont)
step (Ongoing (ApplicationExpression callee args) env store cont) =
  return $ Ongoing callee env store (ApplyContinuation env args [] : cont)
step (Ongoing (LetExpression left (LambdaExpression params body1) body2) env store cont) =
  do
    addr <- Store.init store
    let env' = insert left (Reference addr) env
    save addr (LambdaIndirect env' params body1) store
    return $ Ongoing body2 env' store cont
step (Ongoing (LetExpression left right body) env store cont) =
  return $ Ongoing right env store (LetContinuation env left body : cont)

continue :: (Store s) => Value -> [Continuation] -> s x Address IndirectValue -> ExceptT String (ST x) (State x s)
continue value [] store = return $ Success value store
continue value (LetContinuation env left body : cont) store =
  return $ Ongoing body (insert left value env) store cont
continue value (IfContinuation env consequent alternate : cont) store = do
  test <- branch value
  return $ Ongoing (if test then consequent else alternate) env store cont
continue value (ApplyContinuation _ [] done : cont) store = case done of
  [] -> apply value [] cont store
  (callee : rest) -> apply callee (rest ++ [value]) cont store
continue value (ApplyContinuation env (next : todo) done : cont) store =
  return $ Ongoing next env store (ApplyContinuation env todo (done ++ [value]) : cont)

apply :: (Store s) => Value -> [Value] -> [Continuation] -> s x Address IndirectValue -> ExceptT String (ST x) (State x s)
apply (BuiltinImmediate builtin) input cont store = do
  result <- applyBuiltin builtin input store
  continue result cont store
apply callee@(PrimitiveImmediate _) _ _ _ =
  throwError $ "cannot apply >> " ++ show callee
apply (Reference addr) input cont store = do
  indirect <- load addr store
  applyIndirect indirect input cont store

insertAll :: (Monad m, Ord k, Show k, Show v) => Map k v -> [k] -> [v] -> ExceptT String m (Map k v)
insertAll m ks vs
  | length ks == length vs = return $ union m (fromList $ zip ks vs)
  | otherwise = throwError $ "arity mismatch >> " ++ show ks ++ " >> " ++ show vs

applyIndirect :: IndirectValue -> [Value] -> [Continuation] -> s x Address IndirectValue -> ExceptT String (ST x) (State x s)
applyIndirect (LambdaIndirect env params body) input cont store = do
  env' <- insertAll env params input
  return $ Ongoing body env' store cont
applyIndirect callee input _ _ =
  throwError $ "cannot apply >> " ++ show callee ++ " >> " ++ show input

applyBuiltin :: (Store s) => Builtin -> [Value] -> s x Address IndirectValue -> ExceptT String (ST x) Value
applyBuiltin Eq [v1, v2] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive $ v1 == v2
-- Begin
applyBuiltin Begin vs _ = lastExcept vs
-- Cons
applyBuiltin Cons [v1, v2] store = do
  addr <- Store.init store
  save addr (PairIndirect v1 v2) store
  return $ Reference addr
-- Car
applyBuiltin Car [Reference addr] store =
  fmap fst (load addr store >>= toPair)
-- Cdr
applyBuiltin Cdr [Reference addr] store =
  fmap snd (load addr store >>= toPair)
-- SetCar
applyBuiltin SetCar [Reference addr, v1] store = do
  (_, v2) <- load addr store >>= toPair
  save addr (PairIndirect v1 v2) store
  return $ PrimitiveImmediate NullPrimitive
-- SetCdr
applyBuiltin SetCdr [Reference addr, v2] store = do
  (v1, _) <- load addr store >>= toPair
  save addr (PairIndirect v1 v2) store
  return $ PrimitiveImmediate NullPrimitive
-- IsNull
applyBuiltin IsNull [PrimitiveImmediate NullPrimitive] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsNull _ _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
-- IsBoolean
applyBuiltin IsBoolean [PrimitiveImmediate (BooleanPrimitive _)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsBoolean _ _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
-- IsNumber
applyBuiltin IsNumber [PrimitiveImmediate (NumberPrimitive _)] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsNumber _ _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
-- IsPair
applyBuiltin IsPair [BuiltinImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
applyBuiltin IsPair [PrimitiveImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
applyBuiltin IsPair [Reference addr] store =
  PrimitiveImmediate . BooleanPrimitive . isPair <$> load addr store
-- IsProcedure
applyBuiltin IsProcedure [BuiltinImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive True
applyBuiltin IsProcedure [PrimitiveImmediate _] _ =
  return $ PrimitiveImmediate $ BooleanPrimitive False
applyBuiltin IsProcedure [Reference addr] store =
  PrimitiveImmediate . BooleanPrimitive . isLambda <$> load addr store
-- Apply Primitive Builtin
applyBuiltin builtin arguments _ = do
  input <- mapM unwrapPrimitive arguments
  output <- applyPrimitiveBuiltin builtin input
  return $ PrimitiveImmediate output

toPair :: (Monad m) => IndirectValue -> ExceptT String m (Value, Value)
toPair (PairIndirect x y) = return (x, y)
toPair indirect = throwError $ "exptected a pair but got >> " ++ show indirect

---------------
-- Interface --
---------------

run :: (Store s) => State x s -> ExceptT String (ST x) (Maybe Primitive)
run (Success (PrimitiveImmediate primitive) _) = return $ Just primitive
run (Success _ _) = return Nothing
run state = step state >>= run

toBuiltinBinding :: Builtin -> (Variable, Value)
toBuiltinBinding builtin =
  (Variable $ getBuiltinName builtin, BuiltinImmediate builtin)

initialEnvironment :: Map Variable Value
initialEnvironment =
  fromList
    (map toBuiltinBinding enumBuiltin)

initializeStore :: Int -> ST x (Memory x Address IndirectValue)
initializeStore size = HoleArray <$> newArray (0, size) Nothing

exec :: Int -> Expression -> Either String (Maybe Primitive)
exec memory program = runST $ do
  store <- initializeStore memory
  runExceptT $ run (Ongoing program initialEnvironment store [])
