module Main where

import AdventOfCode (runDay)
import Control.Applicative (asum, optional)
import Control.Arrow ((>>>))
import Control.Lens (Ixed (ix), view, (%~), (&))
import Control.Monad (replicateM)
import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import MegaparsecExtras (Parser, parseNumber, parseOrFail)
import Text.Megaparsec (MonadParsec (try), many, sepEndBy, some)
import Text.Megaparsec.Char (char, letterChar, newline, printChar, string)
import Prelude hiding (lines)

type Crate = Char

type Move = (Int, Int, Int)

parseCrate :: Parser (Maybe Crate)
parseCrate =
  asum
    [ Just <$> (char '[' *> letterChar <* char ']' <* (optional (char ' '))),
      pure Nothing <* replicateM 3 (char ' ') <* (optional (char ' '))
    ]

parseCrateLine :: Parser [Maybe Crate]
parseCrateLine = some parseCrate

parseCrateLines :: Parser [[Crate]]
parseCrateLines = do
  lines <- (try parseCrateLine) `sepEndBy` newline
  return $ map catMaybes $ transpose lines

parseMoveLine :: Parser Move
parseMoveLine = do
  _ <- string "move "
  a <- parseNumber
  _ <- string " from "
  b <- parseNumber
  _ <- string " to "
  c <- parseNumber
  return (a, b, c)

applyMove :: ([Crate] -> [Crate]) -> [[Crate]] -> Move -> [[Crate]]
applyMove onMove l (howMany, from, to) =
  let ixFrom = from - 1
      ixTo = to - 1
      oldFrom = view (ix ixFrom) l
   in l
        & ix ixFrom %~ drop howMany
        & ix ixTo %~ (onMove (take howMany oldFrom) <>)

applyMoves :: ([Crate] -> [Crate]) -> [[Crate]] -> [Move] -> [[Crate]]
applyMoves onMove = foldl' (applyMove onMove)

parse :: String -> ([[Crate]], [Move])
parse = parseOrFail $ do
  stacks <- parseCrateLines
  _ <- many printChar >> newline >> newline
  moves <- parseMoveLine `sepEndBy` newline
  return (stacks, moves)

solvePart1 :: String -> String
solvePart1 = parse >>> uncurry (applyMoves reverse) >>> map head

solvePart2 :: String -> String
solvePart2 = parse >>> uncurry (applyMoves id) >>> map head

main :: IO ()
main = runDay 05 solvePart1 solvePart2
