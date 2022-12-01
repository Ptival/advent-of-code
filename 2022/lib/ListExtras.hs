module ListExtras where

import Data.Bifunctor (bimap)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail l = tail l

splitOnceWhen :: (a -> Bool) -> [a] -> ([a], [a])
splitOnceWhen cond = bimap id safeTail . break cond

-- >>> splitOnceWhen (== 'c') "abcdef"
-- ("ab","def")

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond = go
  where
    go [] = []
    go l = let (h, t) = splitOnceWhen cond l in h : go t

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep = splitWhen (== sep)

-- >>> splitOn 'c' "abcdef"
-- ["ab","def"]
