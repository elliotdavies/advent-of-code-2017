import Test.HUnit
import Lib (sumMatchingConsecutives, sumHalfwayMatches)

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

  TestLabel "sumMatchingConsecutives" $ test [

    "1122" ~: do
      let input = [1,1,2,2]
      let expected = 3
      assertEqual "" expected (sumMatchingConsecutives input)
    ,

    "1111" ~: do
      let input = [1,1,1,1]
      let expected = 4
      assertEqual "" expected (sumMatchingConsecutives input)
    ,

    "1234" ~: do
      let input = [1,2,3,4]
      let expected = 0
      assertEqual "" expected (sumMatchingConsecutives input)
    ,

    "91212129" ~: do
      let input = [9,1,2,1,2,1,2,9]
      let expected = 9
      assertEqual "" expected (sumMatchingConsecutives input)

    ],

  TestLabel "sumHalfwayMatches" $ test [

    "1212" ~: do
      let input = [1,2,1,2]
      let expected = 6
      assertEqual "" expected (sumHalfwayMatches input)
    ,

    "1221" ~: do
      let input = [1,2,2,1]
      let expected = 0
      assertEqual "" expected (sumHalfwayMatches input)
    ,

    "123425" ~: do
      let input = [1,2,3,4,2,5]
      let expected = 4
      assertEqual "" expected (sumHalfwayMatches input)
    ,

    "123123" ~: do
      let input = [1,2,3,1,2,3]
      let expected = 12
      assertEqual "" expected (sumHalfwayMatches input)
    ,

    "12131415" ~: do
      let input = [1,2,1,3,1,4,1,5]
      let expected = 4
      assertEqual "" expected (sumHalfwayMatches input)

    ]
  ]
