module Or where
import Data.Semigroup (mempty, Semigroup, (<>))

data Or a b =
  First a | Second b deriving (Show, Eq)

instance Semigroup (Or a b) where
  First _ <> Second a = Second a
  First _ <> First a = First a
  Second a <> First _ = Second a
  Second a <> Second _ = Second a

instance Monoid a => Monoid (Or a b) where
  mempty = First mempty
  mappend = (<>)

instance Functor (Or e) where
  fmap :: (a -> b) -> (Or e) a -> (Or e) b
  fmap f (Second x) = Second (f x)
  fmap _ (First a) = (First a)

instance Applicative (Or e) where
  pure :: a -> Or e a
  pure x = Second x
  (First f) <*> _ = (First f)
  (Second f) <*> x = fmap f x

instance Monad (Or e) where
  (>>=) :: (Or e) a -> (a -> (Or e) b) -> (Or e) b
  (First x) >>= _ = First x
  (Second x) >>= f = f x
  return = pure

instance Foldable (Or e) where
  foldr :: (a -> b -> b) -> b -> (Or e) a -> b
  foldr f x (Second y) = f y x
  foldr _ x (First _) = x

instance Traversable (Or e) where
  traverse :: Applicative f => (a -> f b) -> Or e a -> f (Or e b)
  traverse f (First x) = pure (First x)
  traverse f (Second x) = fmap Second $ f x
