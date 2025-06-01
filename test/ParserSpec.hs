{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (tests) where

import Data.Text (pack)
import Expression (Expression (ApplicationExpression, IfExpression, LambdaExpression, LetExpression, PrimitiveExpression, VariableExpression))
import Parser (File (File), parseFile)
import Primitive (Primitive (BooleanPrimitive, NullPrimitive, NumberPrimitive))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testParse :: String -> Expression -> TestTree
testParse input program =
  testCase ("parse " ++ input) $
    parseFile (File "main.scm" (pack input))
      @?= Right program

tests :: TestTree
tests =
  testGroup
    "Parser"
    [ testParse " #n " (PrimitiveExpression NullPrimitive),
      testParse " #t " (PrimitiveExpression $ BooleanPrimitive True),
      testParse " #f " (PrimitiveExpression $ BooleanPrimitive False),
      testParse " 123 " (PrimitiveExpression $ NumberPrimitive 123),
      testParse " 123.456 " (PrimitiveExpression $ NumberPrimitive 123.456),
      testParse " xyz " (VariableExpression "xyz"),
      testParse " +-*/?! " (VariableExpression "+-*/?!"),
      testParse
        " ( if x y z ) "
        ( IfExpression
            (VariableExpression "x")
            (VariableExpression "y")
            (VariableExpression "z")
        ),
      testParse
        " ( let x y z ) "
        ( LetExpression
            "x"
            (VariableExpression "y")
            (VariableExpression "z")
        ),
      testParse
        " ( lambda ( x y ) z ) "
        ( LambdaExpression
            ["x", "y"]
            (VariableExpression "z")
        ),
      testParse
        " ( f x y ) "
        ( ApplicationExpression
            (VariableExpression "f")
            [VariableExpression "x", VariableExpression "y"]
        )
    ]
