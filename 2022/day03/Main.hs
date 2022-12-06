module Main where

import AdventOfCode (runDay)
import Control.Arrow ((>>>))
import Data.Char (ord)
import Data.Ix (inRange)
import Data.List (intersect, nub)
import Data.List.Extra (chunksOf)
import MegaparsecExtras (parseOrFail)
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (letterChar, newline)

parse :: String -> [String]
parse = parseOrFail (some letterChar `sepEndBy` newline)

splitHalfway :: String -> (String, String)
splitHalfway s =
  let len = length s `div` 2
   in (take len s, drop len s)

solvePart1 :: String -> Int
solvePart1 =
  parse
    >>> map splitHalfway
    >>> map (nub . uncurry intersect)
    >>> concat
    >>> map priority
    >>> sum

priority :: Char -> Int
priority c
  | inRange (ord 'a', ord 'z') (ord c) = ord c - ord 'a' + 1
  | inRange (ord 'A', ord 'Z') (ord c) = ord c - ord 'A' + 27
  | otherwise = 0

solvePart2 :: String -> Int
solvePart2 =
  parse
    >>> chunksOf 3
    >>> map (priority . head . foldl1 intersect)
    >>> sum

main :: IO ()
main = runDay 03 solvePart1 solvePart2
