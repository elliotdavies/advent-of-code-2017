import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestLabel "tests" $ test [

    TestLabel "parseItems" $ test [

      "{}" ~: do
        let input = "{}"
        let expected = [Group []]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "<<<<>" ~: do
        let input = "<<<<>"
        let expected = [Garbage "<<<"]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "<{!>}>" ~: do
        let input = "<{!>}>"
        let expected = [Garbage "{}"]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "<!!!>>" ~: do
        let input = "<!!!>>"
        let expected = [Garbage ""]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "<{o\"i!a,<{i<a>" ~: do
        let input = "<{o\"i!a,<{i<a>"
        let expected = [Garbage "{o\"i,<{i<a"]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "{{{}}}" ~: do
        let input = "{{{}}}"
        let expected = [Group [Group [Group []]]]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "{{},{}}" ~: do
        let input = "{{},{}}"
        let expected = [Group [Group [], Group []]]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "{<{},{},{{}}>}" ~: do
        let input = "{<{},{},{{}}>}"
        let expected = [Group [Garbage "{},{},{{}}"]]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "{<a>,<a>,<a>,<a>}" ~: do
        let input = "{<a>,<a>,<a>,<a>}"
        let expected = [Group [Garbage "a", Garbage "a", Garbage "a", Garbage "a"]]
        assertEqual "" expected (fst $ parseItems input)
      ,

      "{{<!>},{<!>},{<!>},{<a>}}" ~: do
        let input = "{{<!>},{<!>},{<!>},{<a>}}"
        let expected = [Group [Group [Garbage "},{<},{<},{<a"]]]
        assertEqual "" expected (fst $ parseItems input)

      ]
    ]
