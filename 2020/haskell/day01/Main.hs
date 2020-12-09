{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Tuples

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
