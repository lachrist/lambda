{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Interpreter where

import Continuation (Continuation (ApplyContinuation, IfContinuation, LetContinuation))
import Control.Monad.Except (throwError)
import Control.Monad.ST (ST)
import Control.Monad.Trans.Except (ExceptT)
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
    Variable,
  )
import Primitive (Primitive (Null, PrimitiveBoolean), applyPrimitiveBuiltin)
import Store (Store (init, load, save))
import Value (Address, Builtin (PrimitiveBuiltin, ReferenceBuiltin), IndirectValue (LambdaIndirect, PairIndirect), ReferenceBuiltin (Car, Cdr, Cons, Equal, SetCar, SetCdr), Value (BuiltinImmediate, PrimitiveImmediate, Reference))

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

branch :: (Monad m) => Value -> ExceptT String m Bool
branch (PrimitiveImmediate (PrimitiveBoolean bool)) = return bool
branch _ = throwError "not a boolean"

unwrapPrimitive :: (Monad m) => Value -> ExceptT String m Primitive
unwrapPrimitive (PrimitiveImmediate primitive) = return primitive
unwrapPrimitive value = throwError $ "excepted a builtin >> " ++ show value

step :: (Store s) => State x s -> ExceptT String (ST x) (State x s)
step state@(Success {}) = return state
step (Ongoing (PrimitiveExpression inner) _ store cont) =
  continue (PrimitiveImmediate inner) cont store
step (Ongoing (VariableExpression variable) env store cont) =
  case Data.Map.lookup variable env of
    Nothing -> throwError $ "missing variable :" ++ variable
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
applyBuiltin (ReferenceBuiltin builtin) arguments store =
  applyReferenceBuiltin builtin arguments store
applyBuiltin (PrimitiveBuiltin builtin) arguments _ = do
  input <- mapM unwrapPrimitive arguments
  output <- applyPrimitiveBuiltin builtin input
  return $ PrimitiveImmediate output

toPair :: (Monad m) => IndirectValue -> ExceptT String m (Value, Value)
toPair (PairIndirect x y) = return (x, y)
toPair indirect = throwError $ "exptected a pair but got >> " ++ show indirect

applyReferenceBuiltin :: (Store s) => ReferenceBuiltin -> [Value] -> s x Address IndirectValue -> ExceptT String (ST x) Value
applyReferenceBuiltin Equal [v1, v2] _ =
  return $ PrimitiveImmediate $ PrimitiveBoolean $ v1 == v2
applyReferenceBuiltin Cons [v1, v2] store = do
  addr <- Store.init store
  save addr (PairIndirect v1 v2) store
  return $ Reference addr
applyReferenceBuiltin Car [Reference addr] store =
  fmap fst (load addr store >>= toPair)
applyReferenceBuiltin Cdr [Reference addr] store =
  fmap snd (load addr store >>= toPair)
applyReferenceBuiltin SetCar [Reference addr, v1] store = do
  (_, v2) <- load addr store >>= toPair
  save addr (PairIndirect v1 v2) store
  return $ PrimitiveImmediate Null
applyReferenceBuiltin SetCdr [Reference addr, v2] store = do
  (v1, _) <- load addr store >>= toPair
  save addr (PairIndirect v1 v2) store
  return $ PrimitiveImmediate Null
applyReferenceBuiltin callee input _ =
  throwError $ "cannot apply >> " ++ show callee ++ " >> " ++ show input
