module Trie (
  trieDataList, trieNodeList, buildTrie, trie, trie', trieValueList
) where
import Data.Function ( on )
import Data.Maybe ( mapMaybe )
import Data.List.HT as HT ( groupBy )
import Data.Tree
import Data.Foldable (Foldable(toList))


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

type Trie a b = Tree (a, Maybe b)

-- trie construction helper functions; mostly for debug
trie :: a -> b -> Trie a b
trie a b = Node (a, Just b) []

trie' :: a -> b -> [(a, b)] -> Trie a b
trie' a b blist = Node (a, Just b) (map (uncurry trie) blist)

-- map over data or nodes
trieValueList :: Trie a1 a2 -> [a2]
trieValueList = mapMaybe snd . trieDataList

trieDataList :: Trie a b -> [(a, Maybe b)]
trieDataList = toList

trieNodeList :: Trie a b -> [Trie a b]
trieNodeList = traverse (:[])

-- buildTrie and its helper functions
buildTrie :: (Eq key, Show key, Show val) => [([key], val)] -> [Trie key val]
buildTrie items =
  map buildHelper groups
  where groups = HT.groupBy headCmp items
        headCmp = (==) `on` (head . fst)

        -- groups are always non-empty: buildHelper catches early empty and
        -- one-element lists.
        tail' (_:xs, val) = (xs, val)
        tail' ([], _) = error "can't happen: buildHelper invariant failed"

        buildHelper items' = case items' of
          [] -> error "Cannot build a trie from an empty list"
          -- it can heppen on an empty input line; further it is maintained
          -- to never happen.
          ([], _):_ -> error $ "Failed on an empty key: " ++ show items'
          -- first key is one element key; thus new node will hold its data
          ([k], dat):tailer -> Node (k, Just dat) (buildTrie $ map tail' tailer)
          -- otherwise it is a node without data
          items''@((k:_, _):_) -> Node (k, Nothing) (buildTrie $ map tail' items'')
