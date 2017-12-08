import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

input :: [Lib.Node]
input =
  [ ("pbga", 66, []),
    ("xhth", 57, []),
    ("ebii", 61, []),
    ("havc", 66, []),
    ("ktlj", 57, []),
    ("fwft", 72, ["ktlj", "cntj", "xhth"]),
    ("qoyq", 66, []),
    ("padx", 45, ["pbga", "havc", "qoyq"]),
    ("tknk", 41, ["ugml", "padx", "fwft"]),
    ("jptl", 61, []),
    ("ugml", 68, ["gyxo", "ebii", "jptl"]),
    ("gyxo", 61, []),
    ("cntj", 57, []) ]

tests = TestLabel "tests" $ test [

    TestLabel "findRoot" $ test [

      "example" ~: do
        let expected = "tknk"
        assertEqual "" expected (name $ findRoot input)

      ],

    TestLabel "findImbalance" $ test [

      "example" ~: do
        let expected = 60
        let root = findRoot input
        assertEqual "" expected (findImbalance root input)

      ]
    ]
