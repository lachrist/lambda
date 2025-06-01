import qualified InterpreterSpec
import qualified ParserSpec
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All"
    [ ParserSpec.tests,
      InterpreterSpec.tests
    ]
