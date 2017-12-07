import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

  ]
