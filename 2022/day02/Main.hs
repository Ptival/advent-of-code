{-# LANGUAGE QuasiQuotes #-}

module Main where

import AdventOfCode (contentsForDay)
import Control.Applicative (asum)
import Control.Arrow ((&&&), (>>>))
import Data.Bifunctor (Bifunctor (second), bimap)
import Data.Data (Proxy (Proxy))
import Data.Foldable (asum, find)
import Data.Functor (($>))
import Data.Functor.Compose (Compose)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.String.Interpolate (i, __i)
import Data.Tuple.Extra (dupe)
import Data.Void (Void)
import Debug.Trace
import MegaparsecExtras (Parser, parseOrFail)
import Text.Megaparsec (Parsec, Stream, count, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (asciiChar, char, newline, space)

data RPS = Rock | Paper | Scissors deriving (Eq, Show)

parseTheirs :: Parser RPS
parseTheirs = asum [char 'A' $> Rock, char 'B' $> Paper, char 'C' $> Scissors]

parseMine :: Parser RPS
parseMine = asum [char 'X' $> Rock, char 'Y' $> Paper, char 'Z' $> Scissors]

parseLine :: Parser (RPS, RPS)
parseLine = (,) <$> parseTheirs <* space <*> parseMine

parse :: String -> [(RPS, RPS)]
parse = parseOrFail (parseLine `sepEndBy` newline)

shapeScore :: RPS -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

data Outcome = Lose | Draw | Win

outcomeScore :: Outcome -> Integer
outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win = 6

-- First is theirs, second if mine, outcome from my perspective
rpsOutcome :: (RPS, RPS) -> Outcome
rpsOutcome (a, b) | a == b = Draw
rpsOutcome (Rock, Paper) = Win
rpsOutcome (Paper, Scissors) = Win
rpsOutcome (Scissors, Rock) = Win
rpsOutcome _ = Lose

solvePart1 :: String -> Integer
solvePart1 =
  parse
    >>> map
      ( uncurry (+)
          . bimap (shapeScore . snd) (outcomeScore . rpsOutcome)
          . dupe
      )
    >>> sum

-- * Part 2

parseOutcome :: Parser Outcome
parseOutcome = asum [char 'X' $> Lose, char 'Y' $> Draw, char 'Z' $> Win]

parseLineFixed :: Parser (RPS, Outcome)
parseLineFixed = (,) <$> parseTheirs <* space <*> parseOutcome

parseFixed :: String -> [(RPS, Outcome)]
parseFixed = parseOrFail (parseLineFixed `sepEndBy` newline)

pickPlay :: RPS -> Outcome -> RPS
pickPlay a Draw = a
pickPlay Rock Lose = Scissors
pickPlay Rock Win = Paper
pickPlay Paper Lose = Rock
pickPlay Paper Win = Scissors
pickPlay Scissors Lose = Paper
pickPlay Scissors Win = Rock

solvePart2 :: String -> Integer
solvePart2 =
  parseFixed
    >>> map
      ( uncurry (+)
          . bimap (shapeScore . uncurry pickPlay) (outcomeScore . snd)
          . dupe
      )
    >>> sum

main :: IO ()
main =
  do
    (test_contents, real_contents) <- contentsForDay "02"

    let part1_test = solvePart1 test_contents
    putStrLn [i| Test 1: #{part1_test} |]

    let part1 = solvePart1 real_contents
    putStrLn [i| Part 1: #{part1} |]

    let part2_test = solvePart2 test_contents
    putStrLn [i| Test 2: #{part2_test} |]

    let part2 = solvePart2 real_contents
    putStrLn [i| Part 2: #{part2} |]
