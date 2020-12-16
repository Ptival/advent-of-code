{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import AdventOfCode.Between (isBetween)
import Control.Applicative (Applicative (liftA2))
import Control.Arrow ((>>>))
import Control.Lens (makeLenses)
import Control.Lens.Combinators (over, view)
import Control.Monad (void)
import Data.Function (on, (&))
import Data.List (isPrefixOf, transpose)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy, sepEndBy, some)
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char (char, letterChar, newline, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (Either (..))

-- * Type definitions

type Parser = Parsec Void String

type Range = Int -> Bool

data Field = Field
  { _name :: String,
    _range :: Range
  }

makeLenses ''Field

instance Eq Field where
  (==) = (==) `on` view name

type Ticket = [Int]

data Validation
  = NotValidated
  | Validated

data Input (v :: Validation) = Input
  { _fields :: [Field],
    _myTicket :: Ticket,
    _nearbyTickets :: [Ticket]
  }

makeLenses ''Input

instance Show (Input v) where
  show i = show (view myTicket i)

-- * Parsing

singleRange :: Parser Range
singleRange = uncurry isBetween <$> ((,) <$> decimal <* char '-' <*> decimal)

rangeDisjunction :: Parser Range
rangeDisjunction = liftA2 (||) <$> singleRange <* string " or " <*> singleRange

field :: Parser Field
field =
  Field
    <$> (concat <$> some letterChar `sepBy` space) <* string ": "
    <*> rangeDisjunction

ticket :: Parser Ticket
ticket = decimal `sepBy` char ','

input :: Parser (Input 'NotValidated)
input = do
  _fields <- field `sepEndBy` newline
  void $ newline >> string "your ticket:" >> newline
  _myTicket <- ticket <* newline
  void $ newline >> string "nearby tickets:" >> newline
  _nearbyTickets <- ticket `sepEndBy` newline
  return $ Input {_fields, _myTicket, _nearbyTickets}

-- * Problem utilities

-- | Checks whether the given value is invalid for all possible fields.
isInvalidValue :: Input v -> Int -> Bool
isInvalidValue myInput value =
  not (or ((view range >>> ($ value)) <$> view fields myInput))

-- | Checks whether the given ticket has a value that is invalid for all
-- possible fields.
hasInvalidValue :: Input v -> Ticket -> Bool
hasInvalidValue myInput = any (isInvalidValue myInput)

-- | Creates a list where each index contains the list of all values seen at
-- that index in nearby tickets.
gatherValuesByPosition :: Input v -> [[Int]]
gatherValuesByPosition = view nearbyTickets >>> transpose

-- | Given a list of values, retrieves the fields that accept all those values.
canBeOneOfTheseFields :: Input v -> [Int] -> [Field]
canBeOneOfTheseFields myInput values = filter canBeThisField (view fields myInput)
  where
    canBeThisField :: Field -> Bool
    canBeThisField myField = all (view range myField) values

-- | Removes all tickets that have invalid values from the input.
discardInvalidTickets :: Input v -> Input 'Validated
discardInvalidTickets myInput = over nearbyTickets discardInvalid myInput
  where
    discardInvalid :: [Ticket] -> [Ticket]
    discardInvalid = filter (not . hasInvalidValue myInput)

-- | Returns a list containing, at each index, the fields that may correspond to
-- that index for all valid tickets.
computePossibleFieldsByPosition :: Input 'Validated -> [[Field]]
computePossibleFieldsByPosition myInput =
  gatherValuesByPosition myInput
    & map (canBeOneOfTheseFields myInput)

-- | Given a list of possibilities at each index, resolves the constraint that
-- each index contains exactly one distinct value. Could be improved greatly for
-- performance.
resolveConstraints :: forall a. Eq a => [[a]] -> [a]
resolveConstraints constraints
  | maximum (map length constraints) == 1 = map head constraints
  | otherwise = resolveConstraints (map stepResolve constraints)
  where
    stepResolve :: [a] -> [a]
    stepResolve l
      | length l == 1 = l
      | otherwise = filter (not . (`elem` constraints) . pure) l

-- | Given a list of fields, returns the indices of those whose name start with
-- "departure".
indicesOfDepartureFields :: [Field] -> [Int]
indicesOfDepartureFields =
  zip [0 ..]
    >>> filter (("departure" `isPrefixOf`) . view name . snd)
    >>> map fst

-- * Part 1

partOne :: Input v -> Int
partOne myInput =
  view nearbyTickets myInput
    & concat
    & filter (isInvalidValue myInput)
    & sum

-- * Part 2

-- | Ensures we indeed get 6 fields selected.
checkThatSixFieldsWereSelected :: [Int] -> [Int]
checkThatSixFieldsWereSelected l
  | length l == 6 = l
  | otherwise = error [__i|A number of fields not equal to 6 was selected: #{length l}|]

-- | Multiplies the values from selected fields in my ticket.
multiplyFieldsFromMyTicket :: Input v -> [Int] -> Int
multiplyFieldsFromMyTicket myInput fieldIndices =
  view myTicket myInput
    & zip [0 ..]
    & filter ((`elem` fieldIndices) . fst)
    & map snd
    & product

partTwo :: Input v -> Int
partTwo myInput =
  discardInvalidTickets myInput
    & computePossibleFieldsByPosition
    & resolveConstraints
    & indicesOfDepartureFields
    & checkThatSixFieldsWereSelected
    & multiplyFieldsFromMyTicket myInput

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day16/input/real.txt"
    let myInput = fromMaybe (Input [] [] []) (parseMaybe input contents)
    let solution1 = partOne myInput
    let solution2 = partTwo myInput
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

testString :: String
testString =
  [__i|class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
|]

testInput :: Input 'NotValidated
testInput = fromMaybe (Input [] [] []) (parseMaybe input testString)

-- >>> partOne testInput
-- 71
