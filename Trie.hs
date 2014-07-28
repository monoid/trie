module Trie (Trie, trieDataList, trieNodeList, buildTrie) where
import Data.Function
import Data.List
import Data.Maybe


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
