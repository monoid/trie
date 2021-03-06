module Trie (Trie, trieDataList, trieNodeList, buildTrie) where
import Data.Function
import Data.List
import Data.Maybe


-- Trie that maps [a] to b.  For example, Trie Char Int maps strings to ints.
-- Internal node may either hold or do not hold data:
--
-- a: 10
-- a b c: 20
--
-- Then root node a will hold 10, node a b will hold no data. and a b c will
-- hold 20.
--
-- [Trie a b] is a list of children.
--
-- We keep piece of key in a node, but it would be better to keep some kind of
-- map from piece of key to child node instead.
-- Because of this decision, buildTrie returns list of nodes, not a Trie,
-- because root node has no piece of key.
-- But that's enough for our modest task.
data Trie a b = Trie a (Maybe b) [Trie a b]
              deriving Show

-- trie construction helper functions; mostly for debug
trie a b = Trie a (Just b) []
trie' a b blist = Trie a (Just b) (map (uncurry trie) blist)


-- map over data or nodes
trieDataList = catMaybes . map (\(Trie _ b _) -> b) . trieNodeList
                                   
trieNodeList n@(Trie a b children) = ([n] ++
                                      concatMap trieNodeList children)


-- buildTrie and its helper functions
buildTrie :: (Eq key, Show key, Show val) => [([key], val)] -> [Trie key val]
buildTrie items =
  map buildHelper groups
  where groups = groupBy headCmp items
        headCmp = (==) `on` (head . fst)

        tail' ((_:xs), val) = (xs, val)

        buildHelper items = case items of
          ([], _):_ -> error $ show items
          -- first key is one element key; thus new node will hold its data
          ([k], dat):tailer -> Trie k (Just dat) (buildTrie $ map tail' tailer)
          -- otherwise it is node without data
          items'@(((k:_), _):_) -> Trie k Nothing (buildTrie $ map tail' items')
