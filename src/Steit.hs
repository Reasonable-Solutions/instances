module Steit where

newtype Steit s a =
  Steit { runSteit :: s -> (a, s) }

instance Functor ( Steit s) where
  fmap :: (a -> b) -> Steit s a -> Steit s b
-- Steit s :: s -> (a, s)
  fmap f (Steit f') = Steit b'
    where b' = (\(a,s) -> (f a, s)) . f'
    -- wtf did i just do here?
