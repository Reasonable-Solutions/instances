module Items where


data Items a = NoItems | Cons a (Items a) deriving (Show, Eq, Ord)

cat :: Items a -> Items a -> Items a
cat NoItems xs = xs
cat (Cons x xs) ys = Cons x (cat xs ys )

flatten :: Items (Items a ) -> Items a
flatten NoItems = NoItems
flatten (Cons xs xss) = cat xs (flatten xss)

instance Monad (Items) where
  return = pure
  xs >>= f = flatten (fmap f xs)  -- it's flatmap, ok?

instance Functor (Items) where
  fmap f (Cons a as) = Cons (f a) $ fmap f (Cons a as)
  fmap f NoItems = NoItems

instance Applicative (Items) where
  (<*>) NoItems _ = NoItems
  (<*>) (Cons f fs) x = mappend (fmap f x) (fs <*> x)
  pure x = Cons x NoItems

instance Foldable (Items) where
  foldr = undefined


instance Traversable (Items) where
  traverse = undefined

instance Monoid (Items a) where
  mempty = NoItems
  mappend = cat
