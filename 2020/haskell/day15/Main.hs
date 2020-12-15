{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeLenses, over, set, view)
import Data.Function ((&))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (__i)
import Data.Void (Void)
import Text.Megaparsec (Parsec, parseMaybe, sepBy, some)
import Text.Megaparsec.Char (char, digitChar, newline)
import Prelude hiding (Either (..))

-- * Type definitions

type Parser = Parsec Void String

-- For documentation and safety, let's wrap these 'Int's

newtype Turn = Turn {getTurn :: Int} deriving (Eq, Num, Ord, Show)

newtype Play = Play {getPlay :: Int} deriving (Eq, Num, Ord, Show)

type LastSeenLog = M.Map Play Turn

data State = State
  { _currentTurn :: Turn,
    _lastSeenLog :: LastSeenLog,
    _previousTurnPlay :: Play
  }
  deriving (Show)

makeLenses ''State

-- * Parsing

integer :: Parser Int
integer = read <$> some digitChar

plays :: Parser [Play]
plays = (Play <$> integer) `sepBy` char ',' <* newline

-- * General utilities

-- * Problem utilities

initialState :: State
initialState =
  State
    { _currentTurn = 1,
      _lastSeenLog = M.empty,
      _previousTurnPlay = - 1
    }

-- | Performs the given next play.
-- NOTE: we insert the previous turn play into the log rather than the current
-- turn play, so that we have access to the second previous occurrence of that
-- play.
play :: State -> Play -> State
play s p =
  s
    & over currentTurn (+ 1)
    & over lastSeenLog (M.insert (view previousTurnPlay s) (view currentTurn s))
    & set previousTurnPlay p

-- | Plays all the given starting plays, ignoring the rules, but building up the
-- proper state.
playStartingPlays :: [Play] -> State
playStartingPlays = foldl' play initialState

-- | Computes and plays the next play, according to the rules.
nextPlay :: State -> State
nextPlay s =
  play
    s
    ( maybe
        (Play 0)
        (Play . getTurn . (view currentTurn s -))
        (M.lookup (view previousTurnPlay s) (view lastSeenLog s))
    )

-- | Given a turn to reach, plays all the way to that turn.
playUntilAfterTurn :: Turn -> State -> State
playUntilAfterTurn turn = until ((> turn) . view currentTurn) nextPlay

-- | Returns the play that should be played at the given turn.
getPlayForTurn :: [Play] -> Turn -> Int
getPlayForTurn startingPlays turn = getPlay (view previousTurnPlay finalState)
  where
    finalState = playUntilAfterTurn turn (playStartingPlays startingPlays)

-- * Part 1

-- * Part 2

-- * Executable

main :: IO ()
main =
  do
    contents <- readFile "day15/input/real.txt"
    let myStartingPlays = fromMaybe [] (parseMaybe plays contents)
    let myGetPlayForTurn = getPlayForTurn myStartingPlays
    let solution1 = myGetPlayForTurn 2020
    let solution2 = myGetPlayForTurn 30_000_000
    putStrLn [__i|Problem 1: #{solution1}|]
    putStrLn [__i|Problem 2: #{solution2}|]

-- * Tests

-- >>> partOne [0, 3, 6]
-- 436

-- >>> partOne [1, 3, 2]
-- 1

-- >>> partOne [2, 1, 3]
-- 10

-- >>> partOne [1, 2, 3]
-- 27

-- >>> partOne [2, 3, 1]
-- 78

-- >>> partOne [3, 2, 1]
-- 438

-- >>> partOne [3, 1, 2]
-- 1836
