module Riider where

data Riider a b = R { runRiider :: a -> b}

ask :: Riider a a
ask = R id

-- runRiider is a function
instance Functor (Riider e) where
  fmap :: (a -> b) -> (Riider e) a -> (Riider e) b
  fmap f (R f') = R $ \n -> (f . f') n

instance Applicative (Riider e) where
  pure x = R (\e -> x) -- exactly K
  (R f) <*> (R f') = R ( \e -> (f e) (f' e) ) -- exactly S

instance Monad (Riider e) where
  return = pure
  x >>= f = R $ \e -> runRiider (f (runRiider x e)) e
