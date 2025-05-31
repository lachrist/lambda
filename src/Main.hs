import Data.Text (pack)
import Interpreter (exec)
import Parser (File (File), parseFile)
import Primitive (Primitive)

interprete :: File -> Either String (Maybe Primitive)
interprete file = parseFile file >>= exec

test :: Either String (Maybe Primitive)
test = interprete $ File "main.scm" (pack "42")

main :: IO ()
main = print test
