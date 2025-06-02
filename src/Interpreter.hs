{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- Export State(..) and Continuation (..) to disable warning about unused field selectors
module Interpreter (eval, State (..), Continuation (..)) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Except (ExceptT)
import Data.Array.MArray (newArray)
import Data.Map (Map, fromList, insert, lookup, union)
import Environment (Environment, initialEnvironment)
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
import Memory (HoleArray (HoleArray), Memory (init, load, save))
import Primitive (Primitive)
import Value
  ( Address,
    IndirectValue (LambdaIndirect),
    Value (BuiltinImmediate, PrimitiveImmediate, Reference),
    applyBuiltin,
    toBool,
  )

data Lambda = Lambda
  { environment :: Environment,
    parameters :: [Variable],
    body :: Expression
  }
  deriving (Show)

data Continuation
  = IfContinuation
      { environment :: Environment,
        consequent :: Expression,
        alternate :: Expression
      }
  | LetContinuation
      { environment :: Environment,
        bound :: Variable,
        body :: Expression
      }
  | ApplyContinuation
      { environment :: Environment,
        todo :: [Expression],
        done :: [Value]
      }

type Store x m = m x Address (IndirectValue Lambda)

data State x m
  = Ongoing
      { control :: Expression,
        env :: Environment,
        store :: Store x m,
        cont :: [Continuation]
      }
  | Success {completion :: Value}

----------
-- Step --
----------

step :: (Memory m) => State x m -> ExceptT String (ST x) (State x m)
step state@(Success {}) = return state
step (Ongoing (PrimitiveExpression inner) _ store cont) =
  continue (PrimitiveImmediate inner) cont store
step (Ongoing (VariableExpression variable) env store cont) =
  case Data.Map.lookup variable env of
    Nothing -> throwError $ "missing variable :" ++ show variable
    Just value -> continue value cont store
step (Ongoing (LambdaExpression params body) env store cont) =
  do
    addr <- Memory.init store
    save addr (LambdaIndirect (Lambda env params body)) store
    continue (Reference addr) cont store
step (Ongoing (IfExpression test consequent alternate) env store cont) =
  return $ Ongoing test env store (IfContinuation env consequent alternate : cont)
step (Ongoing (ApplicationExpression callee args) env store cont) =
  return $ Ongoing callee env store (ApplyContinuation env args [] : cont)
step (Ongoing (LetExpression left (LambdaExpression params body1) body2) env store cont) =
  do
    addr <- Memory.init store
    let env' = insert left (Reference addr) env
    save addr (LambdaIndirect (Lambda env' params body1)) store
    return $ Ongoing body2 env' store cont
step (Ongoing (LetExpression left right body) env store cont) =
  return $ Ongoing right env store (LetContinuation env left body : cont)

continue :: (Memory m) => Value -> [Continuation] -> Store x m -> ExceptT String (ST x) (State x m)
continue value [] _ = return $ Success value
continue value (LetContinuation env left body : cont) store =
  return $ Ongoing body (insert left value env) store cont
continue value (IfContinuation env consequent alternate : cont) store = do
  test <- toBool value
  return $ Ongoing (if test then consequent else alternate) env store cont
continue value (ApplyContinuation _ [] done : cont) store = case done of
  [] -> apply value [] cont store
  (callee : rest) -> apply callee (rest ++ [value]) cont store
continue value (ApplyContinuation env (next : todo) done : cont) store =
  return $ Ongoing next env store (ApplyContinuation env todo (done ++ [value]) : cont)

apply :: (Memory m) => Value -> [Value] -> [Continuation] -> Store x m -> ExceptT String (ST x) (State x m)
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
  | otherwise = throwError $ "Arity mismatch >> " ++ show ks ++ " >> " ++ show vs

applyIndirect :: IndirectValue Lambda -> [Value] -> [Continuation] -> Store x m -> ExceptT String (ST x) (State x m)
applyIndirect (LambdaIndirect (Lambda env params body)) input cont store = do
  env' <- insertAll env params input
  return $ Ongoing body env' store cont
applyIndirect callee input _ _ =
  throwError $ "Cannot apply " ++ show callee ++ " onto " ++ show input

----------
-- Eval --
-----------

exec :: (Memory m) => State x m -> ExceptT String (ST x) (Maybe Primitive)
exec (Success (PrimitiveImmediate primitive)) = return $ Just primitive
exec (Success _) = return Nothing
exec state = step state >>= exec

initializeStore :: Int -> ST x (HoleArray x Address (IndirectValue Lambda))
initializeStore size = HoleArray <$> newArray (0, size) Nothing

eval :: Int -> Expression -> Either String (Maybe Primitive)
eval memory program = runST $ do
  store <- initializeStore memory
  runExceptT $ exec (Ongoing program initialEnvironment store [])
