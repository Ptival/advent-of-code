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

solvePart1 :: String -> IO String
solvePart1 =
  caloriesPerElf
    >>> maximum
    >>> pure . show

solvePart2 :: String -> IO String
solvePart2 =
  caloriesPerElf
    >>> sortBy (comparing Down)
    >>> take 3
    >>> sum
    >>> pure . show

main :: IO ()
main = runDay 01 solvePart1 solvePart2
