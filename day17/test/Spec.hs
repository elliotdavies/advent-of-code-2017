import           Lib
import           Test.HUnit

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

    "something" ~: do
      let input = "something"
      let expected = 0
      assertEqual "" expected (fst $ parseItems input)

  ]
