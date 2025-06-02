{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Bytecode.Interpreter where

import Bytecode.Instruction (Block (Block), Instruction (..), Label, Program)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.ST (ST)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V
import Expression (Variable)
import Memory (Memory (..))
import Primitive (Primitive (..))
import Value (Address, IndirectValue (..), Value (..), applyBuiltin)

type Environment = Map Variable Value

data Pointer = Pointer {block :: Label, position :: Int}

data Lambda = Lambda
  { environment :: Map Variable Value,
    control :: Label
  }
  deriving (Show)

data Frame = Frame
  { pointer :: Pointer,
    stack :: [Value],
    environment :: Environment
  }

data State x m = State
  { current :: Frame,
    dump :: [Frame],
    program :: Program,
    store :: m x Address (IndirectValue Lambda)
  }

newtype Completion = Completion Value

----------
-- Util --
----------

insertMaybe :: (Ord k) => Maybe k -> v -> Map k v -> Map k v
insertMaybe Nothing _ env = env
insertMaybe (Just key) val env = M.insert key val env

takeSafe :: (Monad m) => Int -> [a] -> ExceptT String m [a]
takeSafe 0 _ = return []
takeSafe n (x : xs) = (x :) <$> takeSafe (n - 1) xs
takeSafe _ [] = throwError "not enough elements to take"

dropSafe :: (Monad m) => Int -> [a] -> ExceptT String m [a]
dropSafe 0 xs = return xs
dropSafe n (_ : xs) = dropSafe (n - 1) xs
dropSafe _ [] = throwError "not enough elements to drop"

-----------
-- Frame --
-----------

makeFrame :: (Monad m) => Label -> [Variable] -> [Value] -> Environment -> ExceptT String m Frame
makeFrame label keys vals env
  | length keys == length vals =
      return $
        Frame
          (Pointer label 0)
          []
          (M.union env (M.fromList $ zip keys vals))
  | otherwise = throwError $ "arity mismatch >> " ++ show keys ++ " >> " ++ show vals

readFrame :: (Monad m) => Variable -> Frame -> ExceptT String m Value
readFrame key (Frame _ _ env) = case M.lookup key env of
  Nothing -> throwError $ "missing variable " ++ show key
  (Just val) -> return val

getFrameEnvironment :: Frame -> Environment
getFrameEnvironment (Frame _ _ env) = env

pushFrameValue :: Value -> Frame -> Frame
pushFrameValue value (Frame pointer stack env) = Frame pointer (value : stack) env

popFrameValue :: (Monad m) => Frame -> ExceptT String m (Value, Frame)
popFrameValue (Frame pointer (top : stack) env) = return (top, Frame pointer stack env)
popFrameValue _ = throwError "cannot pop value from empty stack"

popMultipleFrameValues :: (Monad m) => Int -> Frame -> ExceptT String m ([Value], Frame)
popMultipleFrameValues size (Frame pointer stack env) = do
  tops <- takeSafe size stack
  rest <- dropSafe size stack
  return (tops, Frame pointer rest env)

incrementFramePosition :: Frame -> Frame
incrementFramePosition (Frame (Pointer label position) stack env) =
  Frame (Pointer label (position + 1)) stack env

------------
-- Helper --
------------

nextPointer :: Pointer -> Pointer
nextPointer (Pointer label position) = Pointer label (position + 1)

incrementStatePosition :: State x s -> State x s
incrementStatePosition (State curr dump prog store) =
  State (incrementFramePosition curr) dump prog store

incrementPosition :: State x s -> State x s
incrementPosition (State (Frame pointer stack env) dump prog store) =
  State (Frame (nextPointer pointer) stack env) dump prog store

loadInstruction :: (Monad m) => Pointer -> Program -> ExceptT String m (Maybe Instruction)
loadInstruction (Pointer label position) prog =
  case M.lookup label prog of
    Nothing -> throwError $ "missing label" ++ show label
    (Just (Block _ body)) -> return $ body V.!? position

branch :: (Monad m) => Value -> ExceptT String m Bool
branch (PrimitiveImmediate (BooleanPrimitive bool)) = return bool
branch _ = throwError "not a boolean"

-----------------
-- Interpreter --
-----------------

step :: (Memory m) => State x m -> ExceptT String (ST x) (Either Completion (State x m))
step state@(State (Frame pointer stack _) dump prog store) =
  loadInstruction pointer prog >>= \case
    Nothing -> case (stack, dump) of
      ([result], []) -> return $ Left $ Completion result
      ([result], current' : dump') -> return $ Right $ State (pushFrameValue result current') dump' prog store
      (_, _) -> throwError "expected a single remaining value on the stack"
    (Just instr) -> Right . incrementStatePosition <$> dispatchInstruction instr state

dispatchInstruction :: (Memory m) => Instruction -> State x m -> ExceptT String (ST x) (State x m)
dispatchInstruction (PrimitiveInstruction prim) (State curr dump prog store) =
  return $ State (pushFrameValue (PrimitiveImmediate prim) curr) dump prog store
dispatchInstruction (LambdaInstruction self label) (State curr dump prog store) = do
  addr <- Memory.init store
  let lambda = Reference addr
  save addr (LambdaIndirect $ Lambda (insertMaybe self lambda (getFrameEnvironment curr)) label) store
  return $ State (pushFrameValue lambda curr) dump prog store
dispatchInstruction (ReadInstruction key) (State curr dump prog store) =
  (\val -> State (pushFrameValue val curr) dump prog store) <$> readFrame key curr
dispatchInstruction (BranchInstruction pos neg) (State curr dump prog store) = do
  (test, curr') <- popFrameValue curr
  fork <- branch test
  goto (if fork then pos else neg) [] (State curr' dump prog store)
dispatchInstruction (CallInstruction arity) (State curr dump prog store) = do
  (args, curr') <- popMultipleFrameValues arity curr
  (callee, curr'') <- popFrameValue curr'
  call callee (reverse args) (State curr'' dump prog store)

goto :: (Memory m) => Label -> [Value] -> State x m -> ExceptT String (ST x) (State x m)
goto label args (State curr dump prog store) =
  case M.lookup label prog of
    Nothing -> throwError $ "missing label" ++ show label
    (Just (Block params _)) ->
      (\curr' -> State curr' (curr : dump) prog store)
        <$> makeFrame label params args (getFrameEnvironment curr)

call :: (Memory m) => Value -> [Value] -> State x m -> ExceptT String (ST x) (State x m)
call (BuiltinImmediate builtin) arguments (State curr dump prog store) =
  (\result -> State (pushFrameValue result curr) dump prog store) <$> applyBuiltin builtin arguments store
call _ _ _ = error "todo"
