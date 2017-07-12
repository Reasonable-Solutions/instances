module Two where
import Data.Semigroup
import Data.Monoid (mempty)

data Two a b = T a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  T a b <> T a' b' = T (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) =>
  Monoid (Two a b) where
  mempty = T mempty mempty
  mappend = (<>)

instance Functor (Two a) where
  fmap f (T x y) = T x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = T mempty x
  (T _ g) <*> (T x y) = T (x) (g y)
