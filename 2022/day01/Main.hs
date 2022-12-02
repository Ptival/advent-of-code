{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Arrow ((>>>))
import Data.List (find, groupBy, sort, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down), comparing)
import Data.String.Interpolate (i)
import ListExtras (splitOn)
import MegaparsecExtras (Parser, lineSeparatedNumbers, parseOrFail)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline)

parse :: String -> [[Integer]]
parse = parseOrFail (lineSeparatedNumbers `sepEndBy` newline)

caloriesPerElf :: String -> [Integer]
caloriesPerElf = parse >>> map sum

solvePart1 :: String -> Integer
solvePart1 = caloriesPerElf >>> maximum

solvePart2 :: String -> Integer
solvePart2 = caloriesPerElf >>> sortBy (comparing Down) >>> take 3 >>> sum

main :: IO ()
main =
  do
    test_contents <- readFile "inputs/day01/test.txt"
    real_contents <- readFile "inputs/day01/real.txt"

    let part1_test = solvePart1 test_contents
    putStrLn [i| Test 1: #{part1_test} |]

    let part1 = solvePart1 real_contents
    putStrLn [i| Part 1: #{part1} |]

    let part2_test = solvePart2 test_contents
    putStrLn [i| Test 2: #{part2_test} |]

    let part2 = solvePart2 real_contents
    putStrLn [i| Part 2: #{part2} |]
