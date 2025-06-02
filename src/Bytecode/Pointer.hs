module Bytecode.Pointer (makeInitialPointer, Pointer, incrementPointer, loadInstruction, loadParameterList) where

import Bytecode.Instruction (Block (..), Instruction, Label, Program)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V
import Expression (Variable)

data Pointer = Pointer Label Int deriving (Show)

makeInitialPointer :: Label -> Pointer
makeInitialPointer label = Pointer label 0

incrementPointer :: Pointer -> Pointer
incrementPointer (Pointer label position) = Pointer label (position + 1)

lookupExcept :: (Monad m, Ord k, Show k) => k -> Map k v -> ExceptT String m v
lookupExcept key env =
  case M.lookup key env of
    Nothing -> throwError $ "Missing label: " ++ show key
    (Just val) -> return val

loadParameterList :: (Monad m) => Pointer -> Program -> ExceptT String m [Variable]
loadParameterList (Pointer label _) prog =
  lookupExcept label prog <&> (\(Block params _) -> params)

loadInstruction :: (Monad m) => Pointer -> Program -> ExceptT String m (Maybe Instruction)
loadInstruction (Pointer label position) prog =
  lookupExcept label prog <&> (\(Block _ body) -> body V.!? position)
