module Bytecode.Compiler (compile) where

import Bytecode.Instruction (Block (Block), Instruction (..), Label (..), Program (..), ProgramBody)
import Control.Monad.Writer (MonadWriter (tell), Writer, runWriter)
import qualified Data.DList as DL
import Data.Functor ((<&>))
import Data.Hashable (Hashable (hash))
import qualified Data.Map as M
import qualified Data.Vector as V
import Expression (Expression (..), Variable)

compileInstruction :: Expression -> Writer ProgramBody (DL.DList Instruction)
compileInstruction (PrimitiveExpression prim) =
  return $ DL.singleton $ PrimitiveInstruction prim
compileInstruction (VariableExpression var) =
  return $ DL.singleton $ ReadInstruction var
compileInstruction (IfExpression test thenBranch elseBranch) = do
  code <- compileInstruction test
  thenLabel <- compileBlock [] thenBranch
  elseLabel <- compileBlock [] elseBranch
  return $ DL.snoc code (IfInstruction thenLabel elseLabel)
compileInstruction (LambdaExpression params body) = do
  bodyLabel <- compileBlock params body
  return $ DL.singleton $ LambdaInstruction Nothing bodyLabel
compileInstruction (LetExpression left (LambdaExpression params lambdaBody) letBody) = do
  lambdaBodyLabel <- compileBlock params lambdaBody
  bodyLabel <- compileBlock [left] letBody
  return $
    DL.append
      (DL.singleton $ LambdaInstruction (Just left) lambdaBodyLabel)
      (DL.singleton $ LetInstruction bodyLabel)
compileInstruction (LetExpression left right body) = do
  rightCode <- compileInstruction right
  bodyLabel <- compileBlock [left] body
  return $ DL.snoc rightCode (LetInstruction bodyLabel)
compileInstruction (ApplicationExpression callee input) = do
  calleeCode <- compileInstruction callee
  inputCode <- mapM compileInstruction input <&> DL.concat
  return $
    calleeCode
      `DL.append` inputCode
      `DL.append` DL.singleton (ApplyInstruction (length input))

compileBlock :: [Variable] -> Expression -> Writer ProgramBody Label
compileBlock params body =
  let label = Label $ fromIntegral $ hash (params, body)
   in do
        code <- compileInstruction body
        tell $ M.fromList [(label, Block params (V.fromList $ DL.toList code))]
        return label

compile :: Expression -> Program
compile main =
  let (entry, body) = runWriter (compileBlock [] main)
   in Program entry body
