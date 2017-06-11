module Main where

import Perhaps
import Or
import Items
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Perhaps a)  where
  arbitrary = fmap Exactly arbitrary

instance ( Arbitrary a, Arbitrary b ) => Arbitrary (Or a b)  where
  arbitrary = oneof [fmap First arbitrary, fmap Second arbitrary ]

instance Arbitrary a  => Arbitrary (Items a) where
  arbitrary =
    oneof [nil, cons]
    where nil = return NoItems
          cons = do
            h <- arbitrary
            tl <- arbitrary
            return $ Cons h tl

instance Eq a => EqProp (Perhaps a) where (=-=) = eq
instance Eq a => EqProp (Items a) where (=-=) = eq
instance ( Eq a, Eq b ) => EqProp (Or a b) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid (undefined :: Perhaps String)
  quickBatch $ monad (undefined :: Perhaps (String, Char, Integer))
  quickBatch $ functor (undefined :: Perhaps (String, Char, Integer))
  quickBatch $ applicative (undefined :: Perhaps (String, Char, Integer))
  quickBatch $ traversable (undefined :: Perhaps (String, Char, [Bool]))

  quickBatch $ applicative (undefined :: Or (String, Char, Integer) (String, Char, Integer))
  quickBatch $ functor (undefined :: Or (String, Char, Integer) (String, Char, Integer))
  quickBatch $ traversable (undefined :: Or (String, Char, [Bool]) (String, Char, [Bool]))
  quickBatch $ monad (undefined :: Or (String, Char, Integer) (String, Char, Integer))

  quickBatch $ monoid (undefined :: Items Integer)
  quickBatch $ monad (undefined :: Items (Integer, Char, Bool))
  quickBatch $ functor (undefined :: Items (Char, Bool, Integer))
  quickBatch $ applicative (undefined :: Items (Char, Bool, Integer))
  quickBatch $ traversable (undefined :: Items (Char, Integer, [Bool]))
