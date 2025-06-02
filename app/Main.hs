{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Bytecode.Compiler (compile)
import Bytecode.Formatter (format)
import qualified Bytecode.Interpreter
import qualified Bytecode.Parser
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import File (File (..))
import Interpreter (eval)
import Parser (parseFile)
import Primitive (Primitive, formatPrimitive)
import System.Environment (getArgs)

usage :: Text
usage =
  T.unlines
    [ "Usage: lambda (eval|comp|exec) <path>",
      "  eval: direct evaluation (CESK machine)",
      "  comp: compile to bytecode",
      "  exec: execute bytecode (SECD machine)"
    ]

main :: IO ()
main =
  getArgs >>= \case
    ["eval", path] -> do
      content <- TIO.readFile path
      putStrLn $
        printCompletion $
          parseFile (File path content)
            >>= Interpreter.eval 10000
    ["comp", path] ->
      ( TIO.readFile path
          >>= ( \case
                  Left message -> putStrLn message
                  Right program ->
                    TIO.writeFile
                      (toBytecodePath path)
                      (format (compile program))
              )
            . parseFile
            . File path
      )
    ["exec", path] -> do
      content <- TIO.readFile path
      putStrLn $
        printCompletion $
          Bytecode.Parser.parseFile (File path content)
            >>= Bytecode.Interpreter.eval 10000
    _ -> TIO.putStrLn usage

toBytecodePath :: String -> String
toBytecodePath path = path ++ ".lambda"

printCompletion :: Either String (Maybe Primitive) -> String
printCompletion (Left message) = message
printCompletion (Right (Just prim)) = formatPrimitive prim
printCompletion (Right Nothing) = "#reference"
