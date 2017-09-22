module Free where

data Free f a =
  Pure a
  | Free (f (Free f a)) -- this is recursive

instance Functor f => Functor (Free f) where --refactor this too have less rec. flavour
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa) -- this is recursive too, fittingly

instance Functor f => Applicative (Free f) where
  pure :: a -> (Free f a)
  pure  = Pure
  (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (Pure f) <*> (Pure a) = Pure $ f a
  (Pure f) <*> (Free f') = Free $ fmap (fmap f) f' -- this could be Pure f <*> f' just as well

  -- foo :: f (Free f (a -> b))
  -- bar :: f (Free f a)
  (Free foo) <*> (Free bar) = _foo <*> _bar

instance Functor f => Monad (Free f) where
  return = pure
  (>>=) :: Free f a -> (a -> (Free f b)) -> Free f b
  Pure a >>= f = f a   -- since f gives us a Free f b, `f a` is per definition :: Free f b
--    ^      ^
--    |      |
--    |      \
--    \        :: (a -> Free f b)
--     :: Free f a
--

  Free a >>= f' = _ugh
