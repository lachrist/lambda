{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (tests) where

import Data.Text (Text, pack, unpack)
import Expression
  ( Expression
      ( ApplicationExpression,
        IfExpression,
        LambdaExpression,
        LetExpression,
        PrimitiveExpression,
        VariableExpression
      ),
    Variable (Variable),
  )
import File (File (..))
import Parser (parseFile)
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
      testParser " xyz " (VariableExpression $ Variable "xyz"),
      testParser " +-*/?!=>< " (VariableExpression $ Variable "+-*/?!=><"),
      testParser
        " ( if x y z ) "
        ( IfExpression
            (VariableExpression $ Variable "x")
            (VariableExpression $ Variable "y")
            (VariableExpression $ Variable "z")
        ),
      testParser
        " ( let x y z ) "
        ( LetExpression
            (Variable "x")
            (VariableExpression $ Variable "y")
            (VariableExpression $ Variable "z")
        ),
      testParser
        " ( lambda ( x y ) z ) "
        ( LambdaExpression
            [Variable "x", Variable "y"]
            (VariableExpression $ Variable "z")
        ),
      testParser
        " ( f x y ) "
        ( ApplicationExpression
            (VariableExpression $ Variable "f")
            [VariableExpression $ Variable "x", VariableExpression $ Variable "y"]
        )
    ]
