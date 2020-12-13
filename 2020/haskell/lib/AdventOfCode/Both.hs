module AdventOfCode.Both (both) where

import Data.Bifunctor (Bifunctor, bimap)

-- | Both 'first' and 'second'.
both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f
