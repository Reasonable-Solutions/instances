module Main where

import Perhaps
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Perhaps a)  where
  arbitrary = fmap Exactly arbitrary

instance Eq a => EqProp (Perhaps a) where (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid (undefined :: Perhaps String)
  quickBatch $ monad (undefined :: Perhaps (String, Char, Integer))
  quickBatch $ functor (undefined :: Perhaps (String, Char, Integer))
  quickBatch $ applicative (undefined :: Perhaps (String, Char, Integer))
  quickBatch $ traversable (undefined :: Perhaps (String, Char, [Bool]))
