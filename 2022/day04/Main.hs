{-# LANGUAGE QuasiQuotes #-}

module Main where

import AdventOfCode (runDay)
import Control.Arrow ((>>>))
import Data.String.Interpolate (i)
import MegaparsecExtras (Parser, parseOrFail)
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

type Interval = (Integer, Integer)

parseInterval :: Parser Interval
parseInterval = (,) <$> decimal <* char '-' <*> decimal

parse :: String -> [(Interval, Interval)]
parse =
  parseOrFail $
    ((,) <$> parseInterval <* char ',' <*> parseInterval)
      `sepEndBy` newline

contains :: Interval -> Interval -> Bool
contains (a, b) (c, d) = (a <= c && b >= d) || (c <= a && d >= b)

overlaps :: Interval -> Interval -> Bool
overlaps i1 i2 = (fst i1 <= snd i2) && (fst i2 <= snd i1)

solvePart1 :: String -> Int
solvePart1 = parse >>> filter (uncurry contains) >>> length

solvePart2 :: String -> Int
solvePart2 = parse >>> filter (uncurry overlaps) >>> length

main :: IO ()
main = runDay 04 solvePart1 solvePart2
