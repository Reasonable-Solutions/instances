{-# LANGUAGE InstanceSigs #-}
module Items where

data Items a = NoItems | Cons a (Items a) deriving (Show, Eq, Ord)

flatten :: Items (Items a ) -> Items a
flatten NoItems = NoItems
flatten (Cons xs xss) = mappend xs (flatten xss)

instance Monoid (Items a) where
  mempty :: Items a
  mempty = NoItems
  mappend :: Items a -> Items a -> Items a
  mappend NoItems xs = xs
  mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor (Items) where
  fmap f (Cons a as) = Cons (f a) (fmap f as)
  fmap f NoItems = NoItems

instance Applicative (Items) where
  (<*>) :: Items (a -> b) -> Items a -> Items b
  (<*>) NoItems _ = NoItems
  (<*>) (Cons f fs) x = mappend (fmap f x) (fs <*> x)
  pure x = Cons x NoItems

instance Foldable (Items) where
  foldr :: (a -> b -> b) -> b -> Items a -> b
  foldr f v (Cons x xs) = f x (foldr f v xs) -- also do the tail of the list
  foldr _ v NoItems = v

instance Traversable (Items) where
  sequenceA :: (Applicative f) => Items (f a) -> f (Items a)
  sequenceA NoItems = pure NoItems
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

instance Monad (Items) where
  return = pure
  xs >>= f = flatten $ fmap f xs  -- it's flatmap, ok?
