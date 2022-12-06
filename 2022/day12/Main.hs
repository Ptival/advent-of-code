module Main where

import AdventOfCode (runDay)
import Control.Arrow ((>>>))
import MegaparsecExtras (Parser, parseOrFail)
import Text.Megaparsec (many, sepEndBy)
import Text.Megaparsec.Char (asciiChar, newline)

parseLine :: Parser String
parseLine = many asciiChar

parse :: String -> [String]
parse = parseOrFail (parseLine `sepEndBy` newline)

solvePart1 :: String -> String
solvePart1 = parse >>> show

solvePart2 :: String -> String
solvePart2 = parse >>> show

main :: IO ()
main = runDay 12 solvePart1 solvePart2
