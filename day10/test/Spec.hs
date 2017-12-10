import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

    TestLabel "part1" $ test [

      "something" ~: do
        let input = "something"
        let expected = 0
        assertEqual "" expected (fst $ parseItems input)

      ]
    ]
