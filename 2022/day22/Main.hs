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

solvePart1 :: String -> IO String
solvePart1 = parse >>> pure . show

solvePart2 :: String -> IO String
solvePart2 = parse >>> pure . show

main :: IO ()
main = runDay 22 solvePart1 solvePart2
