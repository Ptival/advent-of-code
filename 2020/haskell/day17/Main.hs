{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Main where

import AdventOfCode.Between (isBetween)
import Control.Arrow ((>>>))
import Control.Comonad (Comonad (..), extend)
import Control.Comonad.Representable.Store (Store, experiment, store)
import Data.Finite (Finite, getFinite, packFinite)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Functor.Compose (Compose)
import Data.Functor.Identity (Identity)
import Data.Functor.Rep (Representable (..))
import Data.List ((\\))
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.String.Interpolate (__i)
import Data.Type.Natural (Four, Nat (..), Nine, SNat (..), Six, Three, Two, sN1, sN15, sN3, sN6, sNatToInt, snat, type (*), type (+))
import qualified Data.Vector.Sized as V
import Data.Void (Void)
import GHC.TypeNats (KnownNat, natVal)
import qualified GHC.TypeNats as GHC
import Text.Megaparsec (Parsec, count, parseMaybe)
import Text.Megaparsec.Char (char, newline)
import Prelude hiding (Either (..))

-- * Type definitions

type Parser = Parsec Void String

data Cube = Active | Inactive deriving (Eq)

instance Show Cube where
  show Active = "#"
  show Inactive = "."

-- | Sadly, vector-sized uses GHC.TypeNats.Nat natural numbers, which are not
-- structural. Because we need to pattern-match on our natural numbers to
-- trigger type-level computation, we use Peano naturals from Data.Type.Natural,
-- and turn them into GHC's Nats just before indexing vectors with this family.
type family ToGHCNat (n :: Nat) :: GHC.Nat where
  ToGHCNat 'Z = 0
  ToGHCNat ('S n) = 1 + ToGHCNat n

-- | Computes a datatype fit for representing a space of 'dimensions'
-- dimensions, with each dimension holding 'size' cube slots.
type family Space (dimensions :: Nat) (size :: Nat) where
  Space 'Z _ = Identity
  Space ('S n) s = Compose (Space n s) (V.Vector (ToGHCNat s))

-- | Given that the original configuration fits within 'initialSize' slots, and
-- we are going to run `cycles` cycles of the rules, computes the actual size
-- of the space needed to perform the whole computation.
type SizeForCycles (initialSize :: Nat) (cycles :: Nat) =
  initialSize + (Two * cycles)

-- | A comonadic 'Store' containing well-sized spaces of cubes.
type SpaceStore (dimensions :: Nat) (size :: Nat) =
  Store (Space dimensions size) Cube

-- | Given a number of dimensions 'dimensions', and a size 'size' that is the
-- same for all these dimensions, computes a coordinate system for that space.
type family Coordinate (dimensions :: Nat) (size :: Nat) where
  Coordinate 'Z _ = ()
  Coordinate ('S d) s = (Coordinate d s, Finite (ToGHCNat s))

-- | Where a 'Coordinate' is absolute and guaranteed in bounds, an offset is a
-- relative position in the given 'dimensions' dimensional space. As such, it
-- admits negative values, and applying an offset to a coordinate may give a
-- coordinate out of bounds.
type family Offset (dimensions :: Nat) where
  Offset 'Z = ()
  Offset ('S d) = (Offset d, Integer)

-- * Parsing

cube :: Parser Cube
cube = asum [char '#' $> Active, char '.' $> Inactive]

vectorOf :: forall size a. KnownNat size => Parser a -> Parser (V.Vector size a)
vectorOf p =
  fromJust . V.fromList <$> count (fromIntegral (natVal (Proxy :: Proxy size))) p

row :: KnownNat size => Parser (V.Vector size Cube)
row = vectorOf cube

rows :: KnownNat size => Parser (V.Vector size (V.Vector size Cube))
rows = vectorOf (row <* newline)

-- * Problem utilities

-- | Offsets the given coordinate by the given offset, respecting the
-- dimensions' boundaries. Returns Nothing if the offset coordinate falls out of
-- any dimension.
offsetCoordinate ::
  KnownNat (ToGHCNat size) =>
  SNat dimensions ->
  Proxy size ->
  Coordinate dimensions size ->
  Offset dimensions ->
  Maybe (Coordinate dimensions size)
offsetCoordinate SZ _ () () = Just ()
offsetCoordinate (SS d) ps (cs, c) (rs, r) =
  (,) <$> offsetCoordinate d ps cs rs <*> alterFinite (+ r) c

alterFinite :: KnownNat n => (Integer -> Integer) -> Finite n -> Maybe (Finite n)
alterFinite f = getFinite >>> f >>> packFinite

-- | Returns the "null" offset for the given dimensionality.
selfOffset :: SNat dimensions -> Offset dimensions
selfOffset SZ = ()
selfOffset (SS d) = (selfOffset d, 0)

-- | Given a dimensionality and a distance, computes the list of all offsets
-- under that distance. Includes the null offset.
-- >>> length (offsetsUnderDistance sN3 sN1) == 3 * 3 * 3
-- True
-- >>> length (offsetsUnderDistance sN3 sN2) == 5 * 5 * 5
-- True
offsetsUnderDistance :: SNat dimensions -> SNat distance -> [Offset dimensions]
offsetsUnderDistance SZ _ = [()]
offsetsUnderDistance (SS dimensions) distance =
  [ (a, b)
    | a <- offsetsUnderDistance dimensions distance,
      b <- [- delta .. delta]
  ]
  where
    delta :: Integer
    delta = sNatToInt distance

-- | Returns the offsets of all neighbouring cells for the given dimensionality.
-- >>> length (offsetsForNeighborsOnly [snat| 3 |]) == 3 ^ 3 - 1
-- True
-- >>> length (offsetsForNeighborsOnly [snat| 4 |]) == 3 ^ 4 - 1
-- True
offsetsForNeighborsOnly :: Eq (Offset d) => SNat d -> [Offset d]
offsetsForNeighborsOnly d = offsetsUnderDistance d sN1 \\ [selfOffset d]

-- | Given a list of offsets, and a "center" coordinate, returns the list of
-- coordinates for these offsets from that "center". May return fewer
-- coordinates than offsets when the offsets go out of bounds for any dimension.
coordinatesGivenOffsets ::
  forall dimensions size.
  Eq (Offset dimensions) =>
  KnownNat (ToGHCNat size) =>
  SNat dimensions ->
  Proxy size ->
  [Offset dimensions] ->
  Coordinate dimensions size ->
  [Coordinate dimensions size]
coordinatesGivenOffsets dimensions size offsets fromCube =
  catMaybes
    [ offsetCoordinate dimensions size fromCube offset
      | offset <- offsets
    ]

-- | Set of rules to evolve the universe.
cubeRules ::
  forall dimensions size.
  Eq (Offset dimensions) =>
  KnownNat (ToGHCNat size) =>
  Rep (Space dimensions size) ~ Coordinate dimensions size =>
  Representable (Space dimensions size) =>
  SNat dimensions ->
  Proxy size ->
  SpaceStore dimensions size ->
  Cube
cubeRules dimensions size mySpace
  | currentCube == Active && not (isBetween 2 3 activeNeighborsCount) = Inactive
  | currentCube == Inactive && activeNeighborsCount == 3 = Active
  | otherwise = currentCube
  where
    currentCube :: Cube
    currentCube = extract mySpace

    activeNeighborsCount :: Int
    activeNeighborsCount =
      getCubesAtOffsets dimensions size (offsetsForNeighborsOnly dimensions) mySpace
        & filter (Active ==)
        & length

-- | Given a list of offsets and a space, returns the list of cubes at those
-- offsets from the center of the universe.
getCubesAtOffsets ::
  forall dimensions size.
  Eq (Offset dimensions) =>
  KnownNat (ToGHCNat size) =>
  Rep (Space dimensions size) ~ Coordinate dimensions size =>
  Representable (Space dimensions size) =>
  SNat dimensions ->
  Proxy size ->
  [Offset dimensions] ->
  SpaceStore dimensions size ->
  [Cube]
getCubesAtOffsets dimensions size offsets =
  experiment (coordinatesGivenOffsets dimensions size offsets)

-- | Applies the cube rules once.
applyCubeRules ::
  Representable (Space dimensions size) =>
  Proxy dimensions ->
  Proxy size ->
  (SpaceStore dimensions size -> Cube) ->
  SpaceStore dimensions size ->
  SpaceStore dimensions size
applyCubeRules Proxy Proxy = extend

-- | Computes the space after 'ncycles' iterations of the cube rules.
spaceAfterNcycles ::
  forall dimensions initialSize cycles.
  Eq (Offset dimensions) =>
  KnownNat (ToGHCNat (SizeForCycles initialSize cycles)) =>
  KnownNat (1 + ToGHCNat cycles) =>
  Rep (Space dimensions (SizeForCycles initialSize cycles)) ~ Coordinate dimensions (SizeForCycles initialSize cycles) =>
  Representable (Space dimensions (SizeForCycles initialSize cycles)) =>
  SNat dimensions ->
  Proxy initialSize ->
  Proxy cycles ->
  Finite (ToGHCNat ('S cycles)) ->
  SpaceStore dimensions (SizeForCycles initialSize cycles) ->
  SpaceStore dimensions (SizeForCycles initialSize cycles)
spaceAfterNcycles dimensions Proxy Proxy ncycles fromSpace =
  let size = Proxy :: Proxy (SizeForCycles initialSize cycles)
   in iterate
        (applyCubeRules (Proxy :: Proxy dimensions) size (cubeRules dimensions size))
        fromSpace
        !! fromIntegral ncycles

-- | Returns all the neighbors of the center of the universe. Does **not**
-- include the center.
getNeighborsAroundZero ::
  forall dimensions size.
  Eq (Offset dimensions) =>
  KnownNat (ToGHCNat size) =>
  Rep (Space dimensions size) ~ Coordinate dimensions size =>
  Representable (Space dimensions size) =>
  SNat dimensions ->
  Proxy size ->
  SpaceStore dimensions size ->
  [Cube]
getNeighborsAroundZero dimensions size =
  getCubesAtOffsets dimensions size (offsetsForNeighborsOnly dimensions)

-- | Returns all the cubes within the given distance from the center of the
-- universe. Includes the center.
getCubesAroundZero ::
  forall dimensions size.
  Eq (Offset dimensions) =>
  KnownNat (ToGHCNat size) =>
  Rep (Space dimensions size) ~ Coordinate dimensions size =>
  Representable (Space dimensions size) =>
  SNat dimensions ->
  SNat size ->
  SpaceStore dimensions size ->
  [Cube]
getCubesAroundZero dimensions distance =
  getCubesAtOffsets dimensions (Proxy :: Proxy size) (offsetsUnderDistance dimensions distance)

-- | Returns the coordinate of the center of the space given its number of
-- dimensions and size for the dimensions.
-- >>> center Refl [snat|3|] [snat|5|] [snat|2|]
-- ((((),finite 4),finite 4),finite 4)
center ::
  forall dimensions initialSize cycles.
  KnownNat (ToGHCNat (SizeForCycles initialSize cycles)) =>
  Rep (Space dimensions (SizeForCycles initialSize cycles))
    ~ Coordinate dimensions (SizeForCycles initialSize cycles) =>
  SNat dimensions ->
  SNat initialSize ->
  SNat cycles ->
  Coordinate dimensions (SizeForCycles initialSize cycles)
center SZ _ _ = ()
center (SS dimensions) initialSize cycles =
  (center dimensions initialSize cycles, centerCoordinate)
  where
    centerCoordinate ::
      KnownNat (ToGHCNat (SizeForCycles initialSize cycles)) =>
      Finite (ToGHCNat (SizeForCycles initialSize cycles))
    centerCoordinate = fromJust (packFinite ((sNatToInt initialSize + 2 * sNatToInt cycles) `div` 2))

-- The outermost dimension corresponds to "x", the innermost layer of the vector.
-- The second outermost dimension corresponds to "y", the outermost layer of the vector.
-- The remaining dimensions are all set to Inactive.
tabulateFromVector ::
  forall lowerDimensions dimensions initialSize cycles.
  dimensions ~ 'S ('S lowerDimensions) =>
  Eq (Coordinate lowerDimensions (SizeForCycles initialSize cycles)) =>
  KnownNat (ToGHCNat initialSize) =>
  KnownNat (ToGHCNat (SizeForCycles initialSize cycles)) =>
  Rep (Space lowerDimensions (SizeForCycles initialSize cycles))
    ~ Coordinate lowerDimensions (SizeForCycles initialSize cycles) =>
  SNat dimensions ->
  SNat initialSize ->
  SNat cycles ->
  V.Vector (ToGHCNat initialSize) (V.Vector (ToGHCNat initialSize) Cube) ->
  Coordinate dimensions (SizeForCycles initialSize cycles) ->
  Cube
tabulateFromVector (SS (SS lowerDimensions)) initialSize cycles v ((lowerDimensionsCoordinates, y), x)
  -- insert the 2D pattern in the centermost hyperplane only
  | lowerDimensionsCoordinates == center lowerDimensions initialSize cycles =
    fromMaybe
      Inactive
      ( V.index
          <$> (V.index v <$> toInitialSizeFinite initialSize cycles y)
          <*> toInitialSizeFinite initialSize cycles x
      )
  -- cubes in all other hyperplanes are initially inactive
  | otherwise = Inactive

makeInitialSpace ::
  forall lowerDimensions dimensions initialSize cycles.
  dimensions ~ 'S ('S lowerDimensions) =>
  Eq (Coordinate lowerDimensions (SizeForCycles initialSize cycles)) =>
  KnownNat (ToGHCNat initialSize) =>
  KnownNat (ToGHCNat (SizeForCycles initialSize cycles)) =>
  Rep (Space lowerDimensions (SizeForCycles initialSize cycles))
    ~ Coordinate lowerDimensions (SizeForCycles initialSize cycles) =>
  Rep (Space dimensions (SizeForCycles initialSize cycles))
    ~ Coordinate dimensions (SizeForCycles initialSize cycles) =>
  Representable (Space dimensions (SizeForCycles initialSize cycles)) =>
  SNat dimensions ->
  SNat initialSize ->
  SNat cycles ->
  V.Vector (ToGHCNat initialSize) (V.Vector (ToGHCNat initialSize) Cube) ->
  SpaceStore dimensions (SizeForCycles initialSize cycles)
makeInitialSpace dimensions initialSize cycles v =
  store
    (tabulateFromVector dimensions initialSize cycles v)
    (center dimensions initialSize cycles)

-- | Given a one-dimension coordinate in a dimension of size (initialSize +
-- cycles), returns its corresponding coordinate in the restricted dimension of
-- size initialSize, if any.
-- E.g. for initialSize = 3, cycles = 1 it maps:
--   0 -> Nothing
--   1 -> Just 0
--   2 -> Just 1
--   3 -> Just 2
--   4 -> Nothing
-- So, given a coordinate in the larger space, we get Nothing if it corresponds
-- to a point outside the smaller space, and Just with a local coordinate for
-- the smaller space if it falls within it.
toInitialSizeFinite ::
  KnownNat (ToGHCNat initialSize) =>
  SNat initialSize ->
  SNat cycles ->
  Finite (ToGHCNat (SizeForCycles initialSize cycles)) ->
  Maybe (Finite (ToGHCNat initialSize))
toInitialSizeFinite _ cycles f = packFinite (getFinite f - sNatToInt cycles)

nthStep ::
  KnownNat (1 + ToGHCNat cycles) =>
  SNat cycles ->
  Integer ->
  Finite (ToGHCNat ('S cycles))
nthStep cycles n
  | n > sNatToInt cycles = error "You are asking for a step beyond what will be correct."
  | otherwise = fromJust (packFinite n)

solve ::
  forall lowerDimensions dimensions initialSize cycles.
  dimensions ~ 'S ('S lowerDimensions) =>
  Eq (Coordinate lowerDimensions (SizeForCycles initialSize cycles)) =>
  Eq (Offset dimensions) =>
  KnownNat (ToGHCNat initialSize) =>
  KnownNat (1 + ToGHCNat cycles) =>
  KnownNat (ToGHCNat (SizeForCycles initialSize cycles)) =>
  Rep (Space lowerDimensions (SizeForCycles initialSize cycles))
    ~ Coordinate lowerDimensions (SizeForCycles initialSize cycles) =>
  Rep (Space dimensions (SizeForCycles initialSize cycles))
    ~ Coordinate dimensions (SizeForCycles initialSize cycles) =>
  Representable (Space dimensions (SizeForCycles initialSize cycles)) =>
  SNat dimensions ->
  SNat initialSize ->
  SNat cycles ->
  SNat (SizeForCycles initialSize cycles) ->
  Finite (ToGHCNat ('S cycles)) ->
  Input initialSize ->
  Int
solve dimensions initialSize cycles size ncycles =
  makeInitialSpace dimensions initialSize cycles
    >>> spaceAfterNcycles dimensions (Proxy :: Proxy initialSize) (Proxy :: Proxy cycles) ncycles
    >>> getCubesAroundZero dimensions size
    >>> filter (== Active)
    >>> length

-- * Input-specific types

-- | The input will be a two-dimensional slice of some hyperplane of our
-- universe. We will place it at the centermost hyperplane for ease of
-- computation.
type Input initialSize =
  V.Vector (ToGHCNat initialSize) (V.Vector (ToGHCNat initialSize) Cube)

-- TODO: There's gotta be a way to reflect the type into the term or vice-versa

type InitialSize = Nine

myInitialSize :: SNat InitialSize
myInitialSize = [snat| 9 |]

type Cycles = Six

myCycles :: SNat Cycles
myCycles = [snat| 6 |]

type Size = SizeForCycles InitialSize Cycles

mySize :: SNat Size
mySize = [snat| 21 |]

-- * Part 1

type PartOneDimensions = Three

partOneDimensions :: SNat PartOneDimensions
partOneDimensions = sN3

partOne :: Input InitialSize -> Int
partOne =
  solve partOneDimensions myInitialSize myCycles mySize (nthStep myCycles 6)

-- * Part 2

type PartTwoDimensions = Four

partTwoDimensions :: SNat PartTwoDimensions
partTwoDimensions = [snat| 4 |]

partTwo :: Input InitialSize -> Int
partTwo =
  solve partTwoDimensions myInitialSize myCycles mySize (nthStep myCycles 6)

-- * Executable

display2D :: Int -> [Cube] -> String
display2D _ [] = ""
display2D size cubes =
  [__i|
#{take size cubes}
#{display2D size (drop size cubes)}
|]

display3D :: Int -> [Cube] -> String
display3D s = display3D' (- (s `div` 2)) s
  where
    display3D' _ _ [] = ""
    display3D' layer size cubes =
      [__i|
    Layer #{layer}
    #{display2D size (take (size * size) cubes)}

    #{display3D' (layer + 1) size (drop (size * size) cubes)}
    |]

main :: IO ()
main =
  do
    contents <- readFile "day17/input/real.txt"
    let myInput :: Input InitialSize =
          fromMaybe
            (V.replicate (V.replicate Inactive))
            (parseMaybe rows contents)
    let solution1 = partOne myInput
    let solution2 = partTwo myInput
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

testString :: String
testString =
  [__i|.\#.
..\#
\#\#\#\n
|]

testInput :: Input Three
testInput = fromMaybe (error "BAD") (parseMaybe (rows :: Parser (Input Three)) testString)

-- >>> testInput
-- Vector [Vector [.,#,.],Vector [.,.,#],Vector [#,#,#]]

testSolve :: Input Three -> Int
testSolve = solve sN3 sN3 sN6 sN15 6

-- >>> testSolve testInput
-- 112
