{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i, __i)
import Data.Void (Void)
import Safe (findJust)
import Text.Megaparsec (Parsec, parseMaybe, sepBy, some, (<|>))
import Text.Megaparsec.Char (char, digitChar, newline)
import Prelude hiding (Either (..))

-- * Type definitions

type Parser = Parsec Void String

newtype BusId = BusId {getBusId :: Integer} deriving (Eq, Ord, Show)

data BusTime = BusTime {getBusTime :: BusId} | NoBusTime deriving (Show)

type Input = (Integer, [BusTime])

-- * Parsing

integer :: Parser Integer
integer = read <$> some digitChar

busTimes :: Parser [BusTime]
busTimes = busOrNoBus `sepBy` char ','
  where
    busOrNoBus :: Parser BusTime
    busOrNoBus = (BusTime . BusId <$> integer) <|> (NoBusTime <$ char 'x')

input :: Parser Input
input = (,) <$> integer <* newline <*> busTimes <* newline

-- * General utilities

-- | 'hasResidueOneModulo m i' returns 'True' when 'i `mod` m == 1`.
hasResidueOneModulo :: Integer -> Integer -> Bool
hasResidueOneModulo modulo = (== 1) . (`mod` modulo)

-- * Problem utilities

justBusIds :: [BusTime] -> [BusId]
justBusIds = concatMap filterBusId
  where
    filterBusId (BusTime busId) = [busId]
    filterBusId NoBusTime = []

-- | Grabs the buses from the input, and pairs them with their offset from the
-- starting time 0.
pairBusNumberWithOffset :: Input -> [(Integer, Integer)]
pairBusNumberWithOffset (_, myBusTimes) = concatMap filterBusTime (zip myBusTimes [0 ..])
  where
    filterBusTime (BusTime busId, offset) = [(getBusId busId, offset)]
    filterBusTime (NoBusTime, _) = []

-- * Part 1

waitTime :: Integer -> BusId -> Integer
waitTime earlierTime (BusId busId) = busId - (earlierTime `mod` busId)

bestBus :: Input -> BusId
bestBus (earlierTime, myBusTimes) =
  minimumBy (compare `on` waitTime earlierTime) (justBusIds myBusTimes)

partOne :: Input -> Integer
partOne myInput@(earlierTime, _) =
  let myBestBus = bestBus myInput
   in getBusId myBestBus * waitTime earlierTime myBestBus

-- * Part 2

-- | We are searching for some time t, such that:
--
-- t = d_1 (mod p_1)
-- t = d_2 (mod p_2)
-- ...
-- t = d_n (mod p_n)
--
-- where the p_is correspond to bus numbers, and the d_is correspond to the
-- wanted time difference for that bus.
--
-- t = sum [ d_i * si * P_i | i <- [1..n]] (mod P)
-- where
--   P = product [ p_i | i <- [1..n]]   the product of all primes
--   Pi = P `div` p_i                   the product of all other primes than p_i
--   si * P_i = 1 (mod p_i)
partTwo :: [(Integer, Integer)] -> Integer
partTwo relevantInput =
  sum (termForPrime <$> relevantInput) `mod` productOfAllPrimes
  where
    productOfAllPrimes :: Integer
    productOfAllPrimes = product (fst <$> relevantInput)

    -- Note: here our 'coeff' ci is such that:
    --   t + c_i = 0 (mod p_i)
    -- so the d_i is actually (- c_i `mod` p_i)
    termForPrime :: (Integer, Integer) -> Integer
    termForPrime (prime, coeff) = (- coeff `mod` prime) * subtermForPrime prime

    subtermForPrime :: Integer -> Integer
    subtermForPrime p =
      findJust
        (hasResidueOneModulo p)
        (multiplesOf (productOfAllPrimes `div` p))

    multiplesOf :: Integer -> [Integer]
    multiplesOf k = [k, k + k ..]

-- >>> partTwo [(17, 0), (13, 2), (19, 3)]
-- 3417

-- >>> partTwo [(67, 0), (7, 1), (59, 2), (61, 3)]
-- 754018

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day13/input/real.txt"
    let myInput = fromMaybe (0, []) (parseMaybe input contents)
    let solution1 = partOne myInput
    let solution2 = partTwo (pairBusNumberWithOffset myInput)
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

testString :: String
testString =
  [i|939
7,13,x,x,59,x,31,19
|]

testInput :: Input
testInput = fromMaybe (0, []) (parseMaybe input testString)

-- >>> bestBus testInput
-- BusId {getBusId = 41}

-- >>> partOne testInput
-- 410
