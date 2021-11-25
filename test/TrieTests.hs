module Main (main) where
import Test.HUnit
import System.Exit ( exitFailure, exitSuccess )
import Trie (trie, buildTrie)

testEmpty = TestCase(
  assertEqual "Single" [trie 1 3] (buildTrie [([1], 3)])
  )

main:: IO ()
main = do
  counts <- runTestTT ( test [
                          testEmpty
                          ])
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
