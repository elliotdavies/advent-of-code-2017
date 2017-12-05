import Test.HUnit
import Lib (checksum)

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

  TestLabel "checksum" $ test [

    "example" ~: do
      let input = [ [5,1,9,5], [7,5,3], [2,4,6,8] ]
      let expected = 18
      assertEqual "" expected (checksum input)

    ]
  ]
