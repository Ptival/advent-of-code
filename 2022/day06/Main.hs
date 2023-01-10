module Main where

import AdventOfCode (runDay)
import Control.Arrow ((>>>))
import Data.List (nub, tails)
import MegaparsecExtras (parseOrFail)
import Text.Megaparsec (many)
import Text.Megaparsec.Char (letterChar, newline)

parse :: String -> String
parse = parseOrFail $ many letterChar <* newline

markerPosition :: Int -> String -> Int
markerPosition markerLength =
  parse
    >>> zip [markerLength ..]
    >>> tails
    >>> dropWhile ((/= markerLength) . length . nub . take markerLength . map snd)
    >>> head
    >>> head
    >>> fst

solvePart1 :: String -> IO String
solvePart1 = pure . show . markerPosition 4

solvePart2 :: String -> IO String
solvePart2 = pure . show . markerPosition 14

main :: IO ()
main = runDay 06 solvePart1 solvePart2
