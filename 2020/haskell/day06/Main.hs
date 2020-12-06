{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative (Alternative (some))
import Data.List (intersect, foldl', nub)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe, sepBy)
import Text.Megaparsec.Char (letterChar, newline)

type Group = [String]
type Parser = Parsec Void String

parseGroup :: Parser Group
parseGroup = many (some letterChar <* newline)

parseInput :: Parser [Group]
parseInput = parseGroup `sepBy` newline

-- >>> countAnyYesForGroup ["abcd", "fad", "de"]
-- 6
countAnyYesForGroup :: Group -> Int
countAnyYesForGroup = length . nub . concat

countAnyYesForAllGroups :: [Group] -> Int
countAnyYesForAllGroups = sum . (countAnyYesForGroup <$>)

-- >>> countAllYesForGroup ["abcdef", "fa", "bdacf"]
-- 2
countAllYesForGroup :: Group -> Int
countAllYesForGroup = length . foldl' intersect ['a'..'z']

countAllYesForAllGroups :: [Group] -> Int
countAllYesForAllGroups = sum . (countAllYesForGroup <$>)

main :: IO ()
main =
  do
    contents <- readFile "day06/input/real.txt"
    let input = fromMaybe [] (parseMaybe parseInput contents)
    putStrLn [__i|Problem 1: #{countAnyYesForAllGroups input}|]
    putStrLn [__i|Problem 1: #{countAllYesForAllGroups input}|]
