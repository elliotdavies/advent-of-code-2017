import Test.HUnit
import Lib (checksum, checksumEvenlyDivisibles)

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

    ],

  TestLabel "evenly divisible" $ test [

    "example" ~: do
      let input = [ [5,9,2,8], [9,4,7,3], [3,8,6,5] ]
      let expected = 9
      assertEqual "" expected (checksumEvenlyDivisibles input)

    ]
  ]
