import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty = Four' mempty mempty mempty mempty
  mappend (Four' a b c d) (Four' a' b' c' d') =
    Four' (mappend a a') (mappend b b') (mappend c c') (mappend d d')

instance (Monoid a, Monoid b) => Semigroup (Four' a b) where
  (<>) = mappend

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    Four' a a a <$> arbitrary

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure a = Four' mempty mempty mempty a
  (Four' x y z f) <*> (Four' x' y' z' v) =
    Four' (x <> x') (y <> y') (z <> z') (f v)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $
    monoid
      (Four'
         (undefined :: Sum Int)
         (undefined :: Sum Int)
         (undefined :: Sum Int)
         (undefined :: Product Int))
  quickBatch $
    functor (undefined :: Four' (Int, String, String) (String, Int, Int))
  quickBatch $
    applicative
      (undefined :: Four' (Product Int, Sum Int, String) (Int, Int, String))
