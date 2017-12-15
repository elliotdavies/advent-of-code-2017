import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

      "1" ~: do
        let input = [(0, 3),(1, 2),(4, 4),(6, 4)]
        let expected = 24
        assertEqual "" expected (severity input)

    ]
