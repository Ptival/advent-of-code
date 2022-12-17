module FreerExtras where

import Control.Lens (ASetter, Over, Profunctor (lmap), set)
import Control.Monad.Freer (Eff, Members)
import Control.Monad.Freer.State (State, get, put)
import Data.Profunctor.Strong (Strong (second'))

assign :: Members '[State s] effs => ASetter s s a b -> b -> Eff effs ()
assign accessor value = get >>= put . set accessor value

modifying ::
  Members '[State s] effs =>
  Over p ((,) r) s s a b -> p a (r, b) -> Eff effs r
modifying l f = do
  (r, s') <- l f <$> get
  put s'
  return r

modifyingReturningOld ::
  Members '[State s] effs =>
  Strong p =>
  Over p ((,) a) s s a b ->
  p a b ->
  Eff effs a
modifyingReturningOld l f = do
  l `modifying` lmap (\a -> (a, a)) (second' f)
