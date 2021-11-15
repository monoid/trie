{-# OPTIONS_GHC -Wno-unused-top-binds #-}
import qualified Data.ByteString.Lazy.Char8 as B
import Trie

record2 :: B.ByteString -> ([B.ByteString], [B.ByteString])
record2 line = (B.split ' ' a, b)
      where (a:b) = B.split '\t' line

yrecord :: B.ByteString -> [B.ByteString]
yrecord = B.split '\t'

-- По неизвестной причине у нас иногда встречаются дубли.
-- Поскольку сами данные нас интересуют мало, а интресуют только ключи,
-- то от дублей можно просто оставить первую строку.
-- filterDups recs = map head $ groupBy ((==) `on` fst) recs

-- STUB
filterDups:: a -> a
filterDups = id

main:: IO ()
main = do
  stdline <- B.getContents -- >>= (return . GZip.decompress)
  let lines' = B.split '\n' stdline
  -- TODO remove empty last line
  let records = filterDups $ map record2 $ filter (not . B.null) lines'
  putStrLn ("Len: " ++ (show $ length $ concatMap trieNodeList $ buildTrie records))
