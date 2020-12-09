module Tuples (tuplesOfSize) where

-- | Returns all lists the size of the first argument, made from elements
-- sampled from the second argument.
-- >>> tuplesOfSize 2 [1, 2, 3, 4]
-- [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
tuplesOfSize :: Integer -> [a] -> [[a]]
tuplesOfSize 0 _ = []
tuplesOfSize _ [] = []
tuplesOfSize 1 (h : t) = [h] : tuplesOfSize 1 t
tuplesOfSize size (h : t) = withHead ++ withoutHead
  where
    withHead = (h :) <$> tuplesOfSize (size - 1) t
    withoutHead = tuplesOfSize size t
