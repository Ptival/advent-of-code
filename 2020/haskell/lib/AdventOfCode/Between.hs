module AdventOfCode.Between (isBetween) where

isBetween :: Ord a => a -> a -> a -> Bool
isBetween lower higher v =
    lower <= v && v <= higher
