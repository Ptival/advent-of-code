{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.String.Interpolate (i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, runParser, (<|>))
import Text.Megaparsec.Char (char, eol)

data Square = Open | Tree

data Slope = Slope
  { down :: Int,
    right :: Int
  }

type Input = [[Square]]

type Parser = Parsec Void String

square :: Parser Square
square = (char '.' $> Open) <|> (char '#' $> Tree)

line :: Parser [Square]
line = ((:) <$> square <*> line) <|> (eol $> [])

-- | NOTE the 'cycle' here, using a lazy infinite list to handle the wraparound
-- for free.
grid :: Parser [[Square]]
grid = ((:) <$> (cycle <$> line) <*> grid) <|> (eof $> [])

stepInput :: Slope -> Input -> Input
stepInput Slope {down, right} rows = drop down (drop right <$> rows)

countSquare :: Square -> Int
countSquare Open = 0
countSquare Tree = 1

countTrees :: Int -> Input -> Slope -> Int
countTrees alreadyCounted rows slope =
  case stepInput slope rows of
    [] -> alreadyCounted
    [] : _ -> error "This should not happen"
    nextRows@((s : _) : _) -> countTrees (alreadyCounted + countSquare s) nextRows slope

solution :: Input -> Slope -> Int
solution = countTrees 0

main :: IO ()
main =
  do
    contents <- readFile "day03/input/real.txt"
    let rows = fromRight [] (runParser grid "day03" contents)
    let solutionForSlope = solution rows
    let solution1 = solutionForSlope Slope {right = 3, down = 1}
    let solution2 =
          product
            [ solutionForSlope Slope {right = 1, down = 1},
              solution1,
              solutionForSlope Slope {right = 5, down = 1},
              solutionForSlope Slope {right = 7, down = 1},
              solutionForSlope Slope {right = 1, down = 2}
            ]
    putStrLn [i| Problem 1: #{solution1} |]
    putStrLn [i| Problem 2: #{solution2} |]
