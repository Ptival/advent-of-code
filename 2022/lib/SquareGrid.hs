module SquareGrid where

import qualified Data.Vector as V

-- | Grid, outside are lines, inside are columns
type Grid a = V.Vector (V.Vector a)

type Coord = (Int, Int)

(!) :: Grid a -> Coord -> a
g ! (l, c) = g V.! l V.! c

minLine :: Grid a -> Int
minLine = const 0

maxLine :: Grid a -> Int
maxLine = subtract 1 . V.length

minCol :: Grid a -> Int
minCol = const 0

maxCol :: Grid a -> Int
maxCol = subtract 1 . V.length . (V.! 0)

positionsNorthFromOutside :: Grid a -> Coord -> [Coord]
positionsNorthFromOutside g (l, c) = [(i, c) | i <- [minLine g .. l - 1]]

positionsNorthFromClosest :: Grid a -> Coord -> [Coord]
positionsNorthFromClosest g = reverse . positionsNorthFromOutside g

positionsSouthFromOutside :: Grid a -> Coord -> [Coord]
positionsSouthFromOutside g = reverse . positionsSouthFromClosest g

positionsSouthFromClosest :: Grid a -> Coord -> [Coord]
positionsSouthFromClosest g (l, c) = [(i, c) | i <- [l + 1 .. maxLine g]]

positionsWestFromOutside :: Grid a -> Coord -> [Coord]
positionsWestFromOutside g (l, c) = [(l, i) | i <- [minCol g .. c - 1]]

positionsWestFromClosest :: Grid a -> Coord -> [Coord]
positionsWestFromClosest g = reverse . positionsWestFromOutside g

positionsEastFromOutside :: Grid a -> Coord -> [Coord]
positionsEastFromOutside g = reverse . positionsEastFromClosest g

positionsEastFromClosest :: Grid a -> Coord -> [Coord]
positionsEastFromClosest g (l, c) = [(l, i) | i <- [c + 1 .. maxCol g]]
