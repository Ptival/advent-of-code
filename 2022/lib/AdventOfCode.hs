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

runDay ::
  Show p1 =>
  Show p2 =>
  -- | Day of the problem
  Integer ->
  -- | Solver for part 1
  (String -> IO p1) ->
  -- | Solver for part 2
  (String -> IO p2) ->
  IO ()
runDay day solvePart1 solvePart2 =
  do
    (test_contents, real_contents) <- contentsForDay $ printf "%02d" day

    part1_test <- solvePart1 test_contents
    putStrLn [i| Test 1: #{show part1_test} |]

    part1 <- solvePart1 real_contents
    putStrLn [i| Part 1: #{show part1} |]

    part2_test <- solvePart2 test_contents
    putStrLn [i| Test 2: #{show part2_test} |]

    part2 <- solvePart2 real_contents
    putStrLn [i| Part 2: #{show part2} |]
