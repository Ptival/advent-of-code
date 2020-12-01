{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)

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

sumTo2020 :: [Integer] -> Bool
sumTo2020 = (==) 2020 . sum

solution :: Integer -> [Integer] -> Integer
solution tupleSize = product . fromJust . find sumTo2020 . tuplesOfSize tupleSize

main :: IO ()
main =
  do
    contents <- readFile "day01/input/real.txt"
    let inputs = map read (words contents)
    putStrLn [i| Problem 1: #{solution 2 inputs} |]
    putStrLn [i| Problem 2: #{solution 3 inputs} |]
