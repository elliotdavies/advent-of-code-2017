import Test.HUnit
import Lib (sumMatchingConsecutives)

main :: IO ()
main = do
  runTestTT tests
  return ()

-- Test the lexer
tests = TestLabel "sumMatchingConsecutives" $ test [

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
    assertEqual "" expected (sumMatchingConsecutives input) ]
