module Main (main) where
import Test.HUnit
import System.Exit
import Trie (Trie(..), buildTrie)

testEmpty = TestCase(
  assertEqual "Single" [(Trie 1 (Just 3) [])] (buildTrie [([1], 3)])
  )

main:: IO ()
main = do
  counts <- runTestTT ( test [
                          testEmpty
                          ])
  if (errors counts + failures counts == 0)
    then exitSuccess
    else exitFailure
