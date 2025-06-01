{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (tests) where

import Data.Text (Text, pack, unpack)
import Expression (Expression (ApplicationExpression, IfExpression, LambdaExpression, LetExpression, PrimitiveExpression, VariableExpression))
import Parser (File (File), parseFile)
import Primitive (Primitive (BooleanPrimitive, NullPrimitive, NumberPrimitive))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testParser :: Text -> Expression -> TestTree
testParser input program =
  testCase (unpack input) $
    parseFile (File "main.scm" input)
      @?= Right program

tests :: TestTree
tests =
  testGroup
    "Parser"
    [ testParser " #n " (PrimitiveExpression NullPrimitive),
      testParser " #t " (PrimitiveExpression $ BooleanPrimitive True),
      testParser " #f " (PrimitiveExpression $ BooleanPrimitive False),
      testParser " 123 " (PrimitiveExpression $ NumberPrimitive 123),
      testParser " 123.456 " (PrimitiveExpression $ NumberPrimitive 123.456),
      testParser " xyz " (VariableExpression "xyz"),
      testParser " +-*/?!=>< " (VariableExpression "+-*/?!=><"),
      testParser
        " ( if x y z ) "
        ( IfExpression
            (VariableExpression "x")
            (VariableExpression "y")
            (VariableExpression "z")
        ),
      testParser
        " ( let x y z ) "
        ( LetExpression
            "x"
            (VariableExpression "y")
            (VariableExpression "z")
        ),
      testParser
        " ( lambda ( x y ) z ) "
        ( LambdaExpression
            ["x", "y"]
            (VariableExpression "z")
        ),
      testParser
        " ( f x y ) "
        ( ApplicationExpression
            (VariableExpression "f")
            [VariableExpression "x", VariableExpression "y"]
        )
    ]
