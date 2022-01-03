import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend (Three' a b c) (Three' d e f) =
    Three' (mappend a d) (mappend b e) (mappend c f)

instance (Monoid a, Monoid b) => Semigroup (Three' a b) where
  (<>) = mappend

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (Three' a f f') <*> (Three' a' v v') = Three' (a <> a') (f v) (f' v')

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $
    monoid
      (Three'
         (undefined :: Sum Int)
         (undefined :: Product Int)
         (undefined :: Product Int))
  quickBatch $
    functor (undefined :: Three' (Int, String, String) (String, Int, Int))
  quickBatch $
    applicative
      (undefined :: Three' (Sum Int, String, Product Int) (Int, Int, String))
