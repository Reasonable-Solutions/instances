{-# LANGUAGE InstanceSigs #-}
module Perhaps where

data Perhaps a = Not | Exactly a deriving (Show, Eq, Ord)

instance Functor (Perhaps) where
  fmap :: (a -> b) -> Perhaps a -> Perhaps b
  fmap f (Exactly a) = Exactly $ f a
  fmap _ Not = Not

instance Applicative (Perhaps) where
  pure :: a -> Perhaps a
  pure a = Exactly a
  (<*>) :: Perhaps (a -> b) -> Perhaps a -> Perhaps b
  (Exactly f) <*> (Exactly b) = Exactly (f b)
  _ <*> _ = Not

instance Monad Perhaps where
  return :: a -> Perhaps a
  return = pure
  (>>=) :: Perhaps a -> (a -> Perhaps b) -> Perhaps b
  Exactly a >>= f = f a
