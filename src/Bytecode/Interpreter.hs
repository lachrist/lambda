{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Bytecode.Interpreter where

import Bytecode.Frame
  ( Frame (Frame),
    extendFrame,
    getFrameEnvironment,
    incrementFramePosition,
    makeFrame,
    makeLambda,
    popFrameValue,
    popMultipleFrameValues,
    pushFrameValue,
    readFrame,
  )
import Bytecode.Instruction (Instruction (..), Label (Label), Program, TextualProgram, loadProgram)
import Bytecode.Lambda (Lambda (Lambda))
import Bytecode.Pointer (loadInstruction, loadParameterList)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newArray)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Text (pack)
import Environment (initialEnvironment)
import Expression (Variable)
import Memory (HoleArray (HoleArray), Memory (..))
import Primitive (Primitive)
import Value (Address, IndirectValue (..), Value (..), applyBuiltin, toBool)

type Environment = Map Variable Value

data State x m = State
  { frame :: Frame,
    dump :: [Frame],
    program :: Program,
    store :: m x Address (IndirectValue Lambda)
  }

leave :: (Memory m) => [Value] -> [Frame] -> Program -> m x Address (IndirectValue Lambda) -> ExceptT String (ST x) (Either Value (State x m))
leave [result] [] _ _ =
  return $ Left result
leave [result] (frame : dump) prog store =
  return $ Right $ State (pushFrameValue result frame) dump prog store
leave stack _ _ _ =
  throwError $ "Expected a single remaining value on the stack, but got: " ++ show stack

enter :: (Memory m) => Frame -> [Value] -> State x m -> ExceptT String (ST x) (State x m)
enter newFrame@(Frame label _ _) args (State oldFrame dump prog store) = do
  params <- loadParameterList label prog
  newFrame' <- extendFrame newFrame params args
  return $ State newFrame' (oldFrame : dump) prog store

step :: (Memory m) => State x m -> ExceptT String (ST x) (Either Value (State x m))
step (State frame@(Frame pointer stack _) dump prog store) =
  loadInstruction pointer prog >>= \case
    Nothing -> leave stack dump prog store
    (Just instr) -> Right <$> dispatchInstruction instr (State (incrementFramePosition frame) dump prog store)

dispatchInstruction :: (Memory m) => Instruction -> State x m -> ExceptT String (ST x) (State x m)
dispatchInstruction (PrimitiveInstruction prim) (State frame dump prog store) =
  return $ State (pushFrameValue (PrimitiveImmediate prim) frame) dump prog store
dispatchInstruction (LambdaInstruction self label) (State frame dump prog store) = do
  addr <- Memory.init store
  save addr (LambdaIndirect $ makeLambda label ((,addr) <$> self) frame) store
  return $ State (pushFrameValue (Reference addr) frame) dump prog store
dispatchInstruction (ReadInstruction key) (State frame dump prog store) =
  (\val -> State (pushFrameValue val frame) dump prog store) <$> readFrame key frame
dispatchInstruction (BranchInstruction pos neg) (State frame dump prog store) = do
  (test, frame') <- popFrameValue frame
  fork <- toBool test
  enter
    (makeFrame (if fork then pos else neg) (getFrameEnvironment frame))
    []
    (State frame' dump prog store)
dispatchInstruction (ApplyInstruction arity) (State frame dump prog store) = do
  (args, frame') <- popMultipleFrameValues arity frame
  (callee, frame'') <- popFrameValue frame'
  apply callee (reverse args) (State frame'' dump prog store)
dispatchInstruction (LetInstruction label) (State frame dump prog store) = do
  (right, frame') <- popFrameValue frame
  enter
    (makeFrame label (getFrameEnvironment frame))
    [right]
    (State frame' dump prog store)

apply :: (Memory m) => Value -> [Value] -> State x m -> ExceptT String (ST x) (State x m)
apply (PrimitiveImmediate primitive) _ _ =
  throwError $ "Cannot apply primitive value: " ++ show primitive
apply (BuiltinImmediate builtin) args (State frame dump prog store) =
  applyBuiltin builtin args store
    <&> (\result -> State (pushFrameValue result frame) dump prog store)
apply (Reference addr) args state@(State _ _ _ store) =
  load addr store >>= \case
    (LambdaIndirect (Lambda label env)) ->
      enter (makeFrame label env) args state
    pair@(PairIndirect _ _) -> throwError $ "Cannot apply pair value: " ++ show pair

----------
-- Eval --
----------

exec :: (Memory m) => Either Value (State x m) -> ExceptT String (ST x) (Maybe Primitive)
exec (Left (PrimitiveImmediate prim)) = return $ Just prim
exec (Left _) = return Nothing
exec (Right ongoing) = step ongoing >>= exec

initializeStore :: Int -> ST x (HoleArray x Address (IndirectValue Lambda))
initializeStore size = HoleArray <$> newArray (0, size) Nothing

eval :: Int -> TextualProgram -> Either String (Maybe Primitive)
eval memory (loadProgram -> prog) = runST $ do
  store <- initializeStore memory
  runExceptT $
    exec $
      Right $
        State
          (makeFrame (Label $ pack "main") initialEnvironment)
          []
          prog
          store
