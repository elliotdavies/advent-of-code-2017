import Test.HUnit
import Lib (largestValue)

main :: IO ()
main = do
  runTestTT tests
  return ()

input =
  [ ("b", "inc", 5,"a", (">", 1)),
    ("a", "inc", 1,"b", ("<", 5)),
    ("c", "dec", -10,"a", (">=", 1)),
    ("c", "inc", -20,"c", ("==", 10)) ]

tests = TestLabel "tests" $ test [

    TestLabel "findRoot" $ test [

      "example" ~: do
        let expected = 1
        assertEqual "" expected (largestValue input)

      ]
    ]
