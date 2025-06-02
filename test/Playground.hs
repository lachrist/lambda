module Playground where

import Bytecode.Compiler (compile)
import Bytecode.Formatter (format)
import Data.Text (pack, unpack)
import Parser (File (..), parseFile)

comp :: String -> IO ()
comp input =
  case parseFile (File "main.scm" (pack input)) of
    Left message -> putStrLn message
    Right expression -> putStrLn $ unpack $ format $ compile expression
