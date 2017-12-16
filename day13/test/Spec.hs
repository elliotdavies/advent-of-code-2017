import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

      "severity" ~: do
        let input = [(0, 3),(1, 2),(4, 4),(6, 4)]
        let expected = 24
        assertEqual "" expected (severity input)
      ,

      "delay" ~: do
        let input = [(0, 3),(1, 2),(4, 4),(6, 4)]
        let expected = 10
        assertEqual "" expected (delay input)

    ]
