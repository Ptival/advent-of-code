{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Arrow (Arrow ((&&&)))
import Control.Category ((>>>))
import Data.Function ((&))
import Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Tuple.Extra (thd3)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (digitChar, newline)

type Parser = Parsec Void String

parseInput :: Parser [Int]
parseInput = (read <$> some digitChar) `sepEndBy` newline

-- * Part 1

-- | Zips an integer list with itself shifted by 1, with a 0 inserted.
zipShift :: [Int] -> [(Int, Int)]
zipShift l = zip l (0 : l)

-- | Computes the jolt differences between all the inputs adapters (sorted),
-- considering a 0-rated socket and an additional device rated 3 higher than the
-- maximum adapter rating.
joltDifferences :: [Int] -> [Int]
joltDifferences joltRatings =
  joltRatings
    & (:) (maximum joltRatings + 3)
    & sort
    & zipShift
    & (uncurry (-) <$>)

-- | Counts and groups consecutive sequences of the same value in the input
-- list.
-- >>> bucketConsecutivesWithCounts [8, 8, 10, 10, 10, 8, 8, 8]
-- [(8,2),(10,3),(8,3)]
bucketConsecutivesWithCounts :: Eq a => Ord a => [a] -> [(a, Int)]
bucketConsecutivesWithCounts =
  NonEmpty.group
    >>> map (NonEmpty.head &&& length)

-- | Computes the jolt differences and bucket them together, ignoring the
-- original order.
bucketSortedJoltDifferences :: [Int] -> [(Int, Int)]
bucketSortedJoltDifferences =
  joltDifferences
    >>> sort
    >>> bucketConsecutivesWithCounts

-- | Computes the jolt differences and bucket them together, without swapping
-- the original order.
bucketUnsortedJoltDifferences :: [Int] -> [(Int, Int)]
bucketUnsortedJoltDifferences =
  joltDifferences
    >>> bucketConsecutivesWithCounts

-- | Computes the product of the number of 1 jolt increments by the number of 3
-- jolt increments.
productOfOneDiffsByThreeDiffs :: [Int] -> Int
productOfOneDiffsByThreeDiffs =
  bucketSortedJoltDifferences
    >>> filter (liftA2 (||) (== 1) (== 3) . fst) -- keep only the buckets for 1 and 3
    >>> (snd <$>)
    >>> product

-- * Part 2

{--

Why tribonacci?

The question I'm trying to answer is how many n-digits binary numbers exist
that do **not** have 3 consecutive zeroes in them.

Let's count them all:

- those that start with a `1` followed by all possible combinations of
  `(n - 1)` digits that do not have 3 consecutive zeroes,

- those that start with a `01` followed by all possible combinations of
  `(n - 2)` digits that do not have 3 consecutive zeroes,

- those that start with a `001` followed by all possible combinations of
  `(n - 3)` digits that do not have 3 consecutive zeroes,

*** Proof that we did not double count ***
- By definition, all elements in a group have distinct suffixes.
- By definition, two elements in different groups have distinct suffixes.

*** Proof that we did not forget any ***
Consider any number that does not have 3 consecutive zeroes in it, and check
its prefix:
- If it starts with `1`, it must be in the first group!
- if it starts with `01`, it must be in the second group,
- if it starts with `001`, it must be in the third group,
- there is no other possibility, as it can **not** start with `000`!

Therefore we have the recurrence relationship:
  f n = f (n - 1) + f (n - 2) + f (n - 3)
Also known at the Tribonacci sequence.

--}

-- |
-- tribonacci 0 = 0
-- tribonacci 1 = 0
-- tribonacci 2 = 1
-- tribonacci (n + 3) = tribonacci (n + 2) + tribonacci (n + 1) + tribonacci n
-- |

-- >>> tribonacci <$> [0..10]
-- [0,0,1,1,2,4,7,13,24,44,81]
tribonacci :: Int -> Int
tribonacci n = thd3 (iterate trib (1, 0, 0) !! n)
  where
    trib :: (Int, Int, Int) -> (Int, Int, Int)
    trib (a, b, c) = (a + b + c, a, b)

-- | NOTE: This solution only works when there are no 2-jolt increments! See
-- bottom of file for a much slower, but correct solution.
countAdapterConfigurations :: [Int] -> Int
countAdapterConfigurations =
  bucketUnsortedJoltDifferences
    >>> filter ((1 ==) . fst)
    >>> map (tribonacci . (+ 2) . snd)
    >>> product

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day10/input/real.txt"
    let input = fromMaybe [] (parseMaybe parseInput contents)
    let solution1 = productOfOneDiffsByThreeDiffs input
    let solution2 = countAdapterConfigurations input
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

testString :: String
testString =
  [__i|
    16
    10
    15
    5
    1
    11
    7
    19
    6
    12
    4
  |]

testInput :: [Int]
testInput = fromMaybe [] (parseMaybe parseInput testString)

-- >>> productOfOneDiffsByThreeDiffs testInput
-- 35

-- >>> countAdapterConfigurations testInput
-- 8

-- * Part 2, initial attempt (very slow on large inputs)

-- | Somehow, for a list made up of only 1s and 3s, this coincides with
-- gathering all the sequences of consecutive ones, computing their length,
-- calculating (tribonacci (length + 2)), and multiplying all these.
-- That is, the number of ways to not take three consecutive digits.
countConfigurations :: [Int] -> Int
countConfigurations joltRatings =
  joltRatings
    & (:) (maximum joltRatings + 3)
    & sort
    & countConfigurationsAfter 0
  where
    countConfigurationsAfter :: Int -> [Int] -> Int
    countConfigurationsAfter _ [] = 1
    countConfigurationsAfter lastSeen (h : t)
      -- if the next number is much larger, we should not have included the previous
      -- one, this is a spurious branch
      | h > lastSeen + 3 = 0
      | otherwise =
        -- unconditionally count sequences where head is included
        countConfigurationsAfter h t
          + if h == lastSeen + 3
            then -- if the gap is 3, there is no choice
              0
            else -- if the gap is < 3, count sequences skipping 'h'
              countConfigurationsAfter lastSeen t

-- 1XXXXX
-- 01XXXX
-- 001XXX
