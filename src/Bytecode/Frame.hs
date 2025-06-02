module Bytecode.Frame where

import Bytecode.Instruction (Label)
import Bytecode.Lambda (Lambda (Lambda))
import Bytecode.Pointer (Pointer, incrementPointer, makeInitialPointer)
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.Map as M
import Expression (Variable)
import Value (Address, Value (Reference))

type Environment = M.Map Variable Value

data Frame = Frame
  { pointer :: Pointer,
    stack :: [Value],
    environment :: Environment
  }

----------
-- Util --
----------

takeSafe :: (Monad m) => Int -> [a] -> ExceptT String m [a]
takeSafe 0 _ = return []
takeSafe n (x : xs) = (x :) <$> takeSafe (n - 1) xs
takeSafe _ [] = throwError "not enough elements to take"

dropSafe :: (Monad m) => Int -> [a] -> ExceptT String m [a]
dropSafe 0 xs = return xs
dropSafe n (_ : xs) = dropSafe (n - 1) xs
dropSafe _ [] = throwError "not enough elements to drop"

------------
-- Access --
------------

makeFrame :: Label -> Environment -> Frame
makeFrame label = Frame (makeInitialPointer label) []

extendFrame :: (Monad m) => Frame -> [Variable] -> [Value] -> ExceptT String m Frame
extendFrame (Frame pointer stack env) keys vals
  | length keys == length vals =
      return $
        Frame
          pointer
          stack
          (env `M.union` M.fromList (zip keys vals))
  | otherwise =
      throwError $
        "Arity mismatch block: "
          ++ show pointer
          ++ " expects: "
          ++ show keys
          ++ " but received: "
          ++ show vals

makeLambda :: Label -> Maybe (Variable, Address) -> Frame -> Lambda
makeLambda label Nothing (Frame _ _ env) = Lambda label env
makeLambda label (Just (self, addr)) (Frame _ _ env) =
  Lambda label (M.insert self (Reference addr) env)

readFrame :: (Monad m) => Variable -> Frame -> ExceptT String m Value
readFrame key (Frame _ _ env) = case M.lookup key env of
  Nothing -> throwError $ "Missing variable: " ++ show key
  (Just val) -> return val

getFrameEnvironment :: Frame -> Environment
getFrameEnvironment (Frame _ _ env) = env

pushFrameValue :: Value -> Frame -> Frame
pushFrameValue value (Frame pointer stack env) = Frame pointer (value : stack) env

popFrameValue :: (Monad m) => Frame -> ExceptT String m (Value, Frame)
popFrameValue (Frame pointer (top : stack) env) = return (top, Frame pointer stack env)
popFrameValue _ = throwError "Cannot pop value from empty stack"

popMultipleFrameValues :: (Monad m) => Int -> Frame -> ExceptT String m ([Value], Frame)
popMultipleFrameValues size (Frame pointer stack env) = do
  tops <- takeSafe size stack
  rest <- dropSafe size stack
  return (tops, Frame pointer rest env)

incrementFramePosition :: Frame -> Frame
incrementFramePosition (Frame pointer stack env) =
  Frame (incrementPointer pointer) stack env
