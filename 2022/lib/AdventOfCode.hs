{-# LANGUAGE QuasiQuotes #-}

module AdventOfCode where

import Data.String.Interpolate (i)

contentsForDay :: String -> IO (String, String)
contentsForDay day =
  (,)
    <$> readFile [i|inputs/day#{day}/test.txt|]
    <*> readFile [i|inputs/day#{day}/real.txt|]
