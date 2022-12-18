module SquareGrid where

import Control.Arrow ((>>>))
import Data.List (intersperse)
import qualified Data.Vector as V

-- | Grid, outside are lines, inside are columns
type Grid a = V.Vector (V.Vector a)

-- NOTE: Not using a `Show` instance here, because `Show` for `Char` will add
-- quotes and it's annoying.
showGrid :: (a -> String) -> Grid a -> String
showGrid showElement =
  V.map (concat . intersperse " " . map showElement . V.toList)
    >>> V.toList
    >>> unlines

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

isValidCoord :: Grid a -> Coord -> Bool
isValidCoord g (l, c) =
  (0 <= l) && (l <= maxLine g) && (0 <= c) && (c <= maxCol g)

-- | Returns the coordinates of neighbor to the north, south, east, and east
fourWayNeighbors :: Grid a -> Coord -> [Coord]
fourWayNeighbors g (r, c) =
  filter (isValidCoord g) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

-- | Returns the coordinates of neighbor to the south and to the east
futureFourWayNeighbors :: Grid a -> Coord -> [Coord]
futureFourWayNeighbors g (r, c) =
  filter (isValidCoord g) [(r + 1, c), (r, c + 1)]


-- | Assigns a unique number to each coordinate, starting with 0 for (0, 0),
-- then proceeding line by line, column by column.
coordIdentifier :: Grid a -> Coord -> Int
coordIdentifier g (l, c) = l * (maxCol g + 1) + c

allCoordsLineByLine :: Grid a -> [Coord]
allCoordsLineByLine g =
  [ (l, c)
    | l <- [0 .. maxLine g],
      c <- [0 .. maxCol g]
  ]
