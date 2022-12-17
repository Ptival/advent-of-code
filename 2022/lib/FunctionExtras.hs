module FunctionExtras where

infixr 1 $>>>

-- | This is just a convenience function for writing `x $>>> f >>> fg` pipelines
-- where `x` is the input value.
($>>>) :: a -> (a -> b) -> b
($>>>) x f = f x
