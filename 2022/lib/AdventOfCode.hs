{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode where

import Data.String.Interpolate (i)
import Debug.Trace (trace)

contentsForDay :: String -> IO (String, String)
contentsForDay day =
  (,)
    <$> readFile [i|inputs/day#{day}/test.txt|]
    <*> readFile [i|inputs/day#{day}/real.txt|]

traceIt :: Show a => a -> a
traceIt a = trace (show a) a
