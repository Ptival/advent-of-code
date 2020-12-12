{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Arrow ((>>>))
import Control.Lens (makeLenses, over, view)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (char, digitChar, newline)
import Prelude hiding (Either (..))

-- * Type definitions

type Parser = Parsec Void String

data Opcode = North | South | East | West | Left | Right | Forward
  deriving (Eq, Show)

data Direction = FacingNorth | FacingEast | FacingSouth | FacingWest
  deriving (Enum, Eq, Show)

type Command = (Opcode, Int)

data Position = Position
  { _longitude :: Int, -- West  ---> East
    _latitude :: Int --   South ---> North
  }
  deriving (Show)

makeLenses ''Position

data State = State
  { _shipDirection :: Direction, -- only used in part 1
    _waypointPosition :: Position, -- only used in part 2
    _shipPosition :: Position
  }
  deriving (Show)

makeLenses ''State

-- * Parsing

opcode :: Parser Opcode
opcode =
  asum
    [ char 'N' $> North,
      char 'S' $> South,
      char 'E' $> East,
      char 'W' $> West,
      char 'L' $> Left,
      char 'R' $> Right,
      char 'F' $> Forward
    ]

command :: Parser Command
command = (,) <$> opcode <*> (read <$> some digitChar)

instructions :: Parser [Command]
instructions = command `sepEndBy` newline

-- * Navigation utilities

flipAngle :: Int -> Int
flipAngle = (360 -)

turnAngle :: Int -> Direction -> Direction
turnAngle angle d = iterate turnRight d !! (angle `div` 90)

turnRight :: Direction -> Direction
turnRight FacingNorth = FacingEast
turnRight FacingEast = FacingSouth
turnRight FacingSouth = FacingWest
turnRight FacingWest = FacingNorth

move :: Direction -> Int -> Position -> Position
move FacingSouth d = over latitude (subtract d)
move FacingNorth d = over latitude (+ d)
move FacingWest d = over longitude (subtract d)
move FacingEast d = over longitude (+ d)

performInstructions :: (Command -> State -> State) -> [Command] -> State -> State
performInstructions commandHandler commands fromState =
  foldl' (flip commandHandler) fromState commands

manhattanDistance :: State -> Int
manhattanDistance s = abs (view latitude p) + abs (view longitude p)
  where
    p = view shipPosition s

initialShipPosition :: Position
initialShipPosition = Position {_latitude = 0, _longitude = 0}

initialWaypointPosition :: Position
initialWaypointPosition = Position {_latitude = 1, _longitude = 10}

initialState :: State
initialState =
  State
    { _shipDirection = FacingEast,
      _shipPosition = initialShipPosition,
      _waypointPosition = initialWaypointPosition
    }

-- | Rotates around the origin.
rotateRight :: Position -> Position
rotateRight p =
  Position
    { _latitude = - view longitude p,
      _longitude = view latitude p
    }

invertPosition :: Position -> Position
invertPosition = over latitude negate >>> over longitude negate

sumPositions :: Position -> Position -> Position
sumPositions p1 p2 =
  Position
    { _latitude = view latitude p1 + view latitude p2,
      _longitude = view longitude p1 + view longitude p2
    }

-- | Multiplies a position, thought as a vector, by some scalar 'c'.
multByCoeff :: Int -> Position -> Position
multByCoeff c = over latitude (* c) >>> over longitude (* c)

distanceAfterInstructions :: (Command -> State -> State) -> [Command] -> Int
distanceAfterInstructions commandHandler commands =
  performInstructions commandHandler commands initialState
    & manhattanDistance

-- * Part 1

-- | Move the ship according to the command.
performShipCommand :: Command -> State -> State
performShipCommand (North, d) = over shipPosition (move FacingNorth d)
performShipCommand (South, d) = over shipPosition (move FacingSouth d)
performShipCommand (East, d) = over shipPosition (move FacingEast d)
performShipCommand (West, d) = over shipPosition (move FacingWest d)
performShipCommand (Left, a) = over shipDirection (turnAngle (flipAngle a))
performShipCommand (Right, a) = over shipDirection (turnAngle a)
performShipCommand (Forward, d) = forward
  where
    forward s = over shipPosition (move (view shipDirection s) d) s

-- * Part 2

-- | Rotates the waypoint clockwise to the requested amound of degrees
-- (expected: 90, 180, 270).
rotateWaypointRight :: Int -> State -> State
rotateWaypointRight degrees = over waypointPosition rotate
  where
    rotate :: Position -> Position
    rotate position = iterate rotateRight position !! (degrees `div` 90)

-- | Moves to the waypoint 'n' times in a row.
moveToWaypoint :: Int -> State -> State
moveToWaypoint n s = over shipPosition moveShip s
  where
    moveShip :: Position -> Position
    moveShip = sumPositions (multByCoeff n (view waypointPosition s))

-- | Move the waypoint or ship according to the command.
performWaypointCommand :: Command -> State -> State
performWaypointCommand (North, d) = over waypointPosition (move FacingNorth d)
performWaypointCommand (South, d) = over waypointPosition (move FacingSouth d)
performWaypointCommand (East, d) = over waypointPosition (move FacingEast d)
performWaypointCommand (West, d) = over waypointPosition (move FacingWest d)
performWaypointCommand (Left, a) = rotateWaypointRight (flipAngle a)
performWaypointCommand (Right, a) = rotateWaypointRight a
performWaypointCommand (Forward, n) = moveToWaypoint n

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day12/input/real.txt"
    let myInstructions = fromMaybe [] (parseMaybe instructions contents)
    let solution1 = distanceAfterInstructions performShipCommand myInstructions
    let solution2 = distanceAfterInstructions performWaypointCommand myInstructions
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

testString :: String
testString =
  [__i|
F10
N3
F7
R90
F11
|]

testInstructions :: [Command]
testInstructions = fromMaybe [] (parseMaybe instructions testString)

-- >>> testInstructions
-- [(Forward,10),(North,3),(Forward,7),(Right,90),(Forward,11)]

-- >>> distanceAfterInstructions performShipCommand testInstructions
-- 25

-- >>> distanceAfterInstructions performWaypointCommand testInstructions
-- 286
