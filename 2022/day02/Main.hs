module Main where

import AdventOfCode (runDay)
import Control.Applicative (asum)
import Control.Arrow ((>>>))
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Data.Tuple.Extra (dupe)
import MegaparsecExtras (Parser, parseOrFail)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char, newline, space)

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

solvePart1 :: String -> IO String
solvePart1 =
  parse
    >>> map
      ( uncurry (+)
          . bimap (shapeScore . snd) (outcomeScore . rpsOutcome)
          . dupe
      )
    >>> sum
    >>> pure . show

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

solvePart2 :: String -> IO String
solvePart2 =
  parseFixed
    >>> map
      ( uncurry (+)
          . bimap (shapeScore . uncurry pickPlay) (outcomeScore . snd)
          . dupe
      )
    >>> sum
    >>> pure . show

main :: IO ()
main = runDay 02 solvePart1 solvePart2
