import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

    TestLabel "parseStream" $ test [

      "{}" ~: do
        let input = "{}"
        let expected = Group []
        assertEqual "" expected (parseStream input)
      ,

      "<<<<>" ~: do
        let input = "<<<<>"
        let expected = Garbage
        assertEqual "" expected (parseStream input)
      ,

      "<{!>}>" ~: do
        let input = "<{!>}>"
        let expected = Garbage
        assertEqual "" expected (parseStream input)
      ,

      "<!!!>>" ~: do
        let input = "<!!!>>"
        let expected = Garbage
        assertEqual "" expected (parseStream input)
      ,

      "<{o\"i!a,<{i<a>" ~: do
        let input = "<{o\"i!a,<{i<a>"
        let expected = Garbage
        assertEqual "" expected (parseStream input)
      ,

      "{{{}}}" ~: do
        let input = "{{{}}}"
        let expected = Group [Group [Group []]]
        assertEqual "" expected (parseStream input)
      ,

      "{{},{}}" ~: do
        let input = "{{},{}}"
        let expected = Group [Group [], Group []]
        assertEqual "" expected (parseStream input)
      ,

      "{<{},{},{{}}>}" ~: do
        let input = "{<{},{},{{}}>}"
        let expected = Group [Garbage]
        assertEqual "" expected (parseStream input)
      ,

      "{<a>,<a>,<a>,<a>}" ~: do
        let input = "{<a>,<a>,<a>,<a>}"
        let expected = Group [Garbage, Garbage, Garbage, Garbage]
        assertEqual "" expected (parseStream input)
      ,

      "{{<!>},{<!>},{<!>},{<a>}}" ~: do
        let input = "{{<!>},{<!>},{<!>},{<a>}}"
        let expected = Group [Group [Garbage]]
        assertEqual "" expected (parseStream input)

      ]
    ]
