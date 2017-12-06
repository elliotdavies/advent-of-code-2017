import Test.HUnit
import Lib (manhattanDistance)

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

  ]
