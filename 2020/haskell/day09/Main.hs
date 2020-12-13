{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import AdventOfCode.Tuples ( tuplesOfSize )
import Data.List (find, tails)
import Data.List.Extra (inits)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy, some)
import Text.Megaparsec.Char (digitChar, newline)

type Parser = Parsec Void String

parseInput :: Parser [Int]
parseInput = (read <$> some digitChar) `sepEndBy` newline

-- * Part 1

type Window = ([Int], Int)

-- >>> windows 2 [1, 2, 3, 4, 5, 6, 7]
-- [([1,2],3),([2,3],4),([3,4],5),([4,5],6),([5,6],7)]
windows :: Int -> [Int] -> [Window]
windows windowSize list =
  let numberOfWindows = length list - windowSize
   in [ (take windowSize (drop i list), list !! (windowSize + i))
        | i <- [0 .. numberOfWindows - 1]
      ]

-- >>> isGoodWindow ([1, 2, 3], 4)
-- True
-- >>> isGoodWindow ([1, 4, 3], 2)
-- False
isGoodWindow :: Window -> Bool
isGoodWindow (pastNumbers, nextNumber) =
  any ((==) nextNumber . sum) (tuplesOfSize 2 pastNumbers)

-- | Returns the number following the first window of size 'windowSize' whose
-- numbers don't add up to the number following the window.
findFirstBadWindow :: Int -> [Int] -> Int
findFirstBadWindow windowSize =
  maybe 0 snd
    . find (not . isGoodWindow)
    . windows windowSize

-- * Part 2

contiguousSubsequences :: [a] -> [[a]]
contiguousSubsequences = concatMap (tail . inits) . tails

-- | Returns the sum of the minimum and the maximum of the contiguous
-- subsequence that sums to the target value.
findSubsequenceSummingTo :: Int -> [Int] -> Int
findSubsequenceSummingTo target input =
  let subList = fromMaybe [0] (find ((==) target . sum) (contiguousSubsequences input))
      (minElement, maxElement) = (minimum subList, maximum subList)
   in minElement + maxElement

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day09/input/real.txt"
    let input = fromMaybe [] (parseMaybe parseInput contents)
    let solution1 = findFirstBadWindow 25 input
    let solution2 = findSubsequenceSummingTo solution1 input
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

testString :: String
testString =
  [__i|
  35
  20
  15
  25
  47
  40
  62
  55
  65
  95
  102
  117
  150
  182
  127
  219
  299
  277
  309
  576
|]

testInput :: [Int]
testInput = fromMaybe [] (parseMaybe parseInput testString)

-- >>> testInput
-- [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]

-- >>> findFirstBadWindow 5 testInput
-- 127

-- >>> findSubsequenceSummingTo 127 testInput
-- 62
