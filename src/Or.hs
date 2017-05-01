{-# LANGUAGE InstanceSigs #-}
module Or where

data Or a b =
  First a | Second b deriving (Show, Eq)

instance Functor (Or e) where
  fmap :: (a -> b) -> (Or z) a -> (Or z) b
  fmap f (Second x) = Second (f x)
  fmap _ (First a) = (First a)

instance Applicative (Or e) where
  pure x = Second x
  (First f) <*> _ = (First f)
  (Second f) <*> x = fmap f x

instance Monad (Or e) where
  (First x) >>= _ = First x
  (Second x) >>= f = f x
  return = pure

instance Foldable (Or a) where
  foldr f x (Second y) = f y x
  foldr _ x (First _) = x

instance Traversable (Or a) where
  traverse f (First x) = pure (First x)
  traverse f (Second x) = fmap Second $ f x
