import Hello (hello)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "MyTests"
    [testCase "HELLO" $ hello @?= "Hello"]
