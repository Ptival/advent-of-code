{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode
  ( runDay,
    traceIt,
  )
where

import Data.String.Interpolate (i)
import Debug.Trace (trace)
import Text.Printf (printf)

contentsForDay :: String -> IO (String, String)
contentsForDay day =
  (,)
    <$> readFile [i|inputs/day#{day}/test.txt|]
    <*> readFile [i|inputs/day#{day}/real.txt|]

traceIt :: Show a => a -> a
traceIt a = trace (show a) a

-- NOTE: I used to make it so that each problem would return a `Show`-able, but
-- when they return a string, we would re-`show` it and it would look ugly.  So
-- instead, the problem is responsible for turning the output into a `String`.
runDay ::
  -- | Day of the problem
  Integer ->
  -- | Solver for part 1
  (String -> IO String) ->
  -- | Solver for part 2
  (String -> IO String) ->
  IO ()
runDay day solvePart1 solvePart2 =
  do
    (test_contents, real_contents) <- contentsForDay $ printf "%02d" day

    part1_test <- solvePart1 test_contents
    putStrLn [i|  Test 1:\n#{part1_test}|]

    part1 <- solvePart1 real_contents
    putStrLn [i|  Part 1:\n#{part1}|]

    part2_test <- solvePart2 test_contents
    putStrLn [i|  Test 2:\n#{part2_test}|]

    part2 <- solvePart2 real_contents
    putStrLn [i|  Part 2:\n#{part2}|]
