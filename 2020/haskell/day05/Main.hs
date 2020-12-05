{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Language.Literals.Binary (readBinary)
import Text.Megaparsec (Parsec, many, parseMaybe, sepBy, (<|>))
import Text.Megaparsec.Char (char, newline)

type Parser = Parsec Void String

parseBoardingPassDigit :: Parser Char
parseBoardingPassDigit =
  asum
    [ (char 'F' <|> char 'L') $> '0',
      (char 'B' <|> char 'R') $> '1'
    ]

parseBoardingPass :: Parser Integer
parseBoardingPass = readBinary <$> many parseBoardingPassDigit

parseBoardingPasses :: Parser [Integer]
parseBoardingPasses = parseBoardingPass `sepBy` newline

maxSeat :: Integer
maxSeat = 2 ^ (7 + 3 :: Integer) -- 7 rows, 3 columns

-- There exists an empty seat such that the seats next to it are not empty. We
-- use the fact that the list of seats is sorted.
realEmptySeat :: [Integer] -> Integer
realEmptySeat seats =
  -- From the list [a, b, c, d, e, f, ...]
  -- This builds the list [(a, b, c), (b, c, d), (c, d, e), ...]
  let threeConsecutiveSeats = zip3 seats (tail seats) (tail (tail seats))
      (_, realSeat, _) = head (filter notNeighbors threeConsecutiveSeats)
   in realSeat
  where
    notNeighbors :: (Integer, Integer, Integer) -> Bool
    notNeighbors (a, b, c) = a + 1 /= b && b + 1 /= c

main :: IO ()
main =
  do
    contents <- readFile "day05/input/real.txt"
    let boardingPasses = fromMaybe [] (parseMaybe parseBoardingPasses contents)
    putStrLn [__i|Problem 1: #{maximum boardingPasses}|]
    let emptySeats = [0 .. maxSeat] \\ boardingPasses
    let realSeat = realEmptySeat emptySeats
    putStrLn [__i|Problem 2: #{realSeat}|]
