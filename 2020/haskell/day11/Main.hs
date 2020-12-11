{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Arrow ((&&&), (>>>))
import Control.Comonad (Comonad (..))
import Control.Comonad.Representable.Store (Store, experiment, peek, store)
import Data.Bifunctor (Bifunctor, bimap)
import Data.Data (Proxy (Proxy))
import Data.Finite (Finite, finites, getFinite, packFinite)
import Data.Foldable (asum, find)
import Data.Functor (($>))
import Data.Functor.Compose (Compose)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.Reflection (reifyNat)
import Data.String.Interpolate (__i)
import qualified Data.Vector.Sized as V
import Data.Void (Void)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Safe (findJust)
import Text.Megaparsec (Parsec, count, parseMaybe)
import Text.Megaparsec.Char (char, newline)

type Parser = Parsec Void String

-- * Type definitions

type Coordinate l c = (Finite l, Finite c)

data Seat = Floor | Empty | Occupied
  deriving (Eq, Show)

type Grid l c = Store (Compose (V.Vector l) (V.Vector c)) Seat

type GridSize = (Int, Int)

type Rule l c = Grid l c -> Seat

type KnownDimensions (l :: Nat) (c :: Nat) = (KnownNat l, KnownNat c)

-- * Parsing

seat :: Parser Seat
seat =
  asum
    [ char '.' $> Floor,
      char 'L' $> Empty,
      char '#' $> Occupied
    ]

line :: forall c. KnownNat c => Parser (V.Vector c Seat)
line = asSizedVector <$> count c seat
  where
    asSizedVector :: [Seat] -> V.Vector c Seat
    asSizedVector = V.fromList >>> fromMaybe (V.replicate Floor)
    c :: Int
    c = fromIntegral (natVal (Proxy :: Proxy c))

grid :: forall l c. KnownDimensions l c => Parser (V.Vector l (V.Vector c Seat))
grid = asSizedVector <$> count l (line <* newline)
  where
    asSizedVector :: [V.Vector c Seat] -> V.Vector l (V.Vector c Seat)
    asSizedVector = V.fromList >>> fromMaybe (V.replicate (V.replicate Floor))
    l :: Int
    l = fromIntegral (natVal (Proxy :: Proxy l))

-- * General utilities

alterFinite :: KnownNat n => (Integer -> Integer) -> Finite n -> Maybe (Finite n)
alterFinite f = getFinite >>> f >>> packFinite

-- | Both 'first' and 'second'.
both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

-- | Returns all 8 offsets for neighboring cells.
eightDirections :: [(Integer, Integer)]
eightDirections =
  [(l, c) | l <- [-1, 0, 1], c <- [-1, 0, 1], (l, c) /= (0, 0)]

-- | Returns the first value 'a' from 'iterate step a' such that 'stop a (step
-- a)' is 'True'.  **Will loop infinitely on non-converging data!**
iterateUntil2 :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntil2 stop step a =
  let iterations = iterate step a
   in fst (findJust (uncurry stop) (zip iterations (tail iterations)))

knownInt :: forall n. KnownNat n => Int
knownInt = fromIntegral (natVal (Proxy :: Proxy n))

-- | Assumes the first line is representative. Very unsafe! <3
numberOfColumns :: String -> Int
numberOfColumns = length . takeWhile (/= '\n')

numberOfLines :: String -> Int
numberOfLines = length . lines

-- | Takes and unwraps all 'Just's until the first 'Nothing'.
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = map fromJust . takeWhile isJust

-- * Grid utilities

-- | Applies the given rules once.
applySeatingRules :: KnownDimensions l c => Rule l c -> Grid l c -> Grid l c
applySeatingRules = extend

-- | A Grid is not immediately observable, but we can prod its cells.
inspectGrid :: forall l c. KnownDimensions l c => Grid l c -> [[Seat]]
inspectGrid myGrid = [[peek (l, c) myGrid | c <- finites] | l <- finites]

countOccupiedSeats :: KnownDimensions l c => Grid l c -> Int
countOccupiedSeats =
  inspectGrid >>> concat >>> filter (== Occupied) >>> length

-- | Repeatedly applies the rules until they leave the grid unchanged.
reachEquilibrium :: forall l c. KnownDimensions l c => Rule l c -> Grid l c -> Grid l c
reachEquilibrium rules = iterateUntil2 (curry compareGrids) (applySeatingRules rules)
  where
    compareGrids :: (Grid l c, Grid l c) -> Bool
    compareGrids = uncurry (==) . both inspectGrid

countOccupiedSeatsAtEquilibrium :: KnownDimensions l c => Rule l c -> Grid l c -> Int
countOccupiedSeatsAtEquilibrium rules =
  reachEquilibrium rules >>> countOccupiedSeats

zero :: forall l c. KnownDimensions l c => Coordinate l c
zero = fromJust ((,) <$> packFinite 0 <*> packFinite 0)

makeGrid :: forall l c. KnownDimensions l c => V.Vector l (V.Vector c Seat) -> Grid l c
makeGrid v = store index2D zero
  where
    index2D :: Coordinate l c -> Seat
    index2D (l, c) = V.index (V.index v l) c

-- | Given the expected size, parses a grid of exactly that size. If the input
-- string is invalid, returns a grid the wanted size but full of 'Floor's!
parseSizedGrid :: KnownDimensions l c => String -> (Proxy l, Proxy c) -> Grid l c
parseSizedGrid input (Proxy, Proxy) =
  maybe (store (const Floor) zero) makeGrid (parseMaybe grid input)

-- | Uses the given pair as the two known dimensions. The types cannot guarantee
-- that we're not swapping 'l' and 'c', so it's best to do the following check:
-- >>> withKnownDimensions (2, 5) (curry (bimap natVal natVal))
-- (2,5)
withKnownDimensions ::
  (Int, Int) ->
  (forall l c. KnownDimensions l c => (Proxy l, Proxy c) -> r) ->
  r
withKnownDimensions (l, c) handler =
  reifyNat (fromIntegral c) (reifyNat (fromIntegral l) (curry handler))

-- * Part 1

-- | Given a cell, returns the list of coordinates for all its neighbors.
-- Returns 8 coordinates for cells in the center of the matrix, but fewer for
-- those at the edges.
neighborCoords :: KnownDimensions l c => Coordinate l c -> [Coordinate l c]
neighborCoords (l, c) =
  catMaybes
    [ (,) <$> alterFinite (+ dl) l <*> alterFinite (+ dc) c
      | dl <- [-1, 0, 1],
        dc <- [-1, 0, 1],
        (dl, dc) /= (0, 0)
    ]

firstSeatingRules :: forall l c. KnownDimensions l c => Rule l c
firstSeatingRules myGrid
  | currentSeat == Empty && occupiedAdjacentSeats == 0 = Occupied
  | currentSeat == Occupied && occupiedAdjacentSeats >= 4 = Empty
  | otherwise = currentSeat
  where
    currentSeat :: Seat
    currentSeat = extract myGrid

    neighbors :: [Seat]
    neighbors = experiment neighborCoords myGrid

    occupiedAdjacentSeats :: Int
    occupiedAdjacentSeats = length (filter (Occupied ==) neighbors)

-- * Part 2

-- | Given an initial direction, and a given cell, returns the list of all cells
-- at repeated applications of that direction from the given cell. Stops
-- correctly when reaching the bounds.
-- >>> coordinatesInDirection (1, 2) zero :: [Coordinate 10 10]
-- [(finite 1,finite 1),(finite 2,finite 2)]
coordinatesInDirection ::
  KnownDimensions l c => (Integer, Integer) -> Coordinate l c -> [Coordinate l c]
coordinatesInDirection (dl, dc) (l, c) =
  takeWhileJust
    [ (,) <$> alterFinite (+ dls) l <*> alterFinite (+ dcs) c
      | dls <- iterate (+ dl) dl
      | dcs <- iterate (+ dc) dc
    ]

secondSeatingRules :: forall l c. KnownDimensions l c => Rule l c
secondSeatingRules myGrid
  | currentSeat == Empty && occupiedVisibleSeats == 0 = Occupied
  | currentSeat == Occupied && occupiedVisibleSeats >= 5 = Empty
  | otherwise = currentSeat
  where
    currentSeat :: Seat
    currentSeat = extract myGrid

    -- for a given direction, grabs all seats in that direction
    seatsInDirection :: (Integer, Integer) -> [Seat]
    seatsInDirection direction = experiment (coordinatesInDirection direction) myGrid

    -- for every direction, get the first non-'Floor' seat, when it exists
    visibleSeats :: [Seat]
    visibleSeats =
      catMaybes [find (/= Floor) (seatsInDirection d) | d <- eightDirections]

    occupiedVisibleSeats :: Int
    occupiedVisibleSeats = length (filter (Occupied ==) visibleSeats)

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day11/input/real.txt"
    let l = numberOfLines contents
    let c = numberOfColumns contents
    let (solution1, solution2) =
          withKnownDimensions
            (l, c)
            ( parseSizedGrid contents
                >>> ( countOccupiedSeatsAtEquilibrium firstSeatingRules
                        &&& countOccupiedSeatsAtEquilibrium secondSeatingRules
                    )
            )
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]
