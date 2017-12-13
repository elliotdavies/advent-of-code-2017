import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

      "countConnections" ~: do
        let input = [(0, [2]), (1, [1]), (2, [0, 3, 4]), (3, [2, 4]), (4, [2, 3, 6]), (5, [6]), (6, [4, 5])]
        let expected = 6
        assertEqual "" expected (countConnections 0 $ parseInput input)
      
    ]
