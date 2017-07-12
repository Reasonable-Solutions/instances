module Two where

data Two a b = T a b

instance Monoid (Two a b) where
 mempty = undefined
 mappend = undefined

instance Functor (Two a) where
  fmap f (T x y) = T x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = T mempty x
  (T _ g) <*> (T x y) = T (x) (g y)
