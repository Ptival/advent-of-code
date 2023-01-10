module Main where

import AdventOfCode (runDay)
import Control.Arrow ((>>>))
import Data.Function ((&))
import qualified Data.Vector as V
import MegaparsecExtras (Parser, parseOrFail)
import qualified SquareGrid as SG
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (newline, numberChar)

parseLine :: Parser [Int]
parseLine = some (read . (: []) <$> numberChar)

parse :: String -> [[Int]]
parse =
  parseOrFail $
    parseLine
      `sepEndBy` newline

type Forest = SG.Grid Int

type Tree = SG.Coord

asForest :: [[Int]] -> Forest
asForest = V.fromList . map V.fromList

treeHeight :: Forest -> Tree -> Int
treeHeight = (SG.!)

-- A tree is visible if one of the slices of its trees to the north, west,
-- south, or east, are all smaller than it.
isVisible :: Forest -> Tree -> Bool
isVisible f t =
  or $
    map
      (all (< treeHeight f t))
      . map (map (treeHeight f))
      $ [ SG.positionsNorthFromClosest f t,
          SG.positionsEastFromClosest f t,
          SG.positionsSouthFromClosest f t,
          SG.positionsWestFromClosest f t
        ]

visiblePositions :: Forest -> [(Int, Int)]
visiblePositions f =
  let nbLines = V.length f
      nbCols = V.length (f V.! 0)
   in [ (l, c)
        | l <- [0 .. nbLines - 1],
          c <- [0 .. nbCols - 1],
          isVisible f (l, c)
      ]

solvePart1 :: String -> IO String
solvePart1 =
  parse
    >>> asForest
    >>> visiblePositions
    >>> length
    >>> pure . show

treeScore :: Forest -> (Int, Int) -> Int
treeScore f t@(line, col) =
  let self = treeHeight f (line, col)
      score :: [SG.Coord] -> Int
      score slice =
        (slice &) $
          map (treeHeight f)
            >>> takeWhile ((< self))
            -- This is yucky
            >>> (\lf -> if length lf == length slice then length slice else length lf + 1)
   in ( [ SG.positionsNorthFromClosest,
          SG.positionsWestFromClosest,
          SG.positionsSouthFromClosest,
          SG.positionsEastFromClosest
        ]
          &
      )
        $ map (($ f) >>> ($ t))
          >>> map score
          >>> product

maxScore :: Forest -> Int
maxScore f =
  maximum
    [ treeScore f (i, j)
      | i <- [SG.minLine f + 1 .. SG.maxLine f - 1], -- Sides are always worth 0, no need to check
        j <- [SG.minCol f + 1 .. SG.maxCol f - 1] -- Likewise
    ]

solvePart2 :: String -> IO String
solvePart2 =
  parse
    >>> asForest
    >>> maxScore
    >>> pure . show

main :: IO ()
main = runDay 08 solvePart1 solvePart2
