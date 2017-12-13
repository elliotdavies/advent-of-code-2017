import Test.HUnit
import Lib

import Math.Geometry.Grid.HexagonalInternal2 (HexDirection(..))

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

      "parseInput" ~: do
        let input = "n,s,ne,se,nw,sw"
        let expected = [North,South,Northeast,Southeast,Northwest,Southwest]
        assertEqual "" expected (parseInput input)
      ,

      "countConnections" ~: do
        let input = "n,s,ne,se,nw,sw"
        let expected = [North,South,Northeast,Southeast,Northwest,Southwest]
        assertEqual "" expected (countConnections input)

    ]
