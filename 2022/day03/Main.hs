{-# LANGUAGE QuasiQuotes #-}

module Main where

import AdventOfCode (contentsForDay)
import Control.Arrow ((>>>))
import Data.Char (ord)
import Data.Ix (inRange)
import Data.List (intersect, nub)
import Data.List.Extra (chunksOf)
import Data.String.Interpolate (i)
import MegaparsecExtras (Parser, parseOrFail)
import Text.Megaparsec (Parsec, Stream, count, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (asciiChar, char, letterChar, newline, space)

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

-- * Part 2

solvePart2 :: String -> Int
solvePart2 =
  parse
    >>> chunksOf 3
    >>> map (priority . head . foldl1 intersect)
    >>> sum

main :: IO ()
main =
  do
    (test_contents, real_contents) <- contentsForDay "03"

    let part1_test = solvePart1 test_contents
    putStrLn [i| Test 1: #{part1_test} |]

    let part1 = solvePart1 real_contents
    putStrLn [i| Part 1: #{part1} |]

    let part2_test = solvePart2 test_contents
    putStrLn [i| Test 2: #{part2_test} |]

    let part2 = solvePart2 real_contents
    putStrLn [i| Part 2: #{part2} |]
