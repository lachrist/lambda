{-# LANGUAGE OverloadedStrings #-}

module InterpreterSpec (tests) where

import Data.Text (Text, unpack)
import Interpreter (exec)
import Parser (File (File), parseFile)
import Primitive (Primitive (BooleanPrimitive, NumberPrimitive))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testInterpreter :: Text -> Primitive -> TestTree
testInterpreter program result =
  testCase (unpack program) $
    (parseFile (File "main.scm" program) >>= exec 10000) @?= Right (Just result)

tests :: TestTree
tests =
  testGroup
    "Interpreter"
    [ testInterpreter "123" (NumberPrimitive 123),
      testInterpreter "(let x 123 x)" (NumberPrimitive 123),
      testInterpreter "(if #t 123 456)" (NumberPrimitive 123),
      testInterpreter "(if #f 123 456)" (NumberPrimitive 456),
      testInterpreter "((lambda (x) x) 123)" (NumberPrimitive 123),
      testInterpreter "(+ 2 3)" (NumberPrimitive 5),
      testInterpreter "(begin 123 456 789)" (NumberPrimitive 789),
      testInterpreter "(car (cons 123 456))" (NumberPrimitive 123),
      testInterpreter "(cdr (cons 123 456))" (NumberPrimitive 456),
      testInterpreter "(eq? 123 123)" (BooleanPrimitive True),
      testInterpreter "(eq? 123 456)" (BooleanPrimitive False),
      testInterpreter "(eq? (cons 123 456) (cons 123 456))" (BooleanPrimitive False),
      testInterpreter "(let x (cons 123 456) (eq? x x))" (BooleanPrimitive True),
      testInterpreter
        "(let x (cons 123 456) (begin (set-car! x 789) (car x)))"
        (NumberPrimitive 789),
      testInterpreter
        "(let x (cons 123 456) (begin (set-cdr! x 789) (cdr x)))"
        (NumberPrimitive 789),
      testInterpreter
        "(let fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))) (fac 6))"
        (NumberPrimitive 720)
    ]
