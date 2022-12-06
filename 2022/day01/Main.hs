module Main where

import AdventOfCode (runDay)
import Control.Arrow ((>>>))
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import MegaparsecExtras (lineSeparatedNumbers, parseOrFail)
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
main = runDay 01 solvePart1 solvePart2
