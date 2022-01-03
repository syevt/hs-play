import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Two a b =
  Two a b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend (Two a b) (Two c d) = Two (mappend a c) (mappend b d)

instance (Monoid a, Monoid b) => Semigroup (Two a b) where
  (<>) = mappend

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (Two f g) <*> (Two x y) = Two (f <> x) (g y)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid (Two (undefined :: Sum Int) (undefined :: String))
  quickBatch $ functor (undefined :: Two (Int, String, Int) (Int, Int, String))
  quickBatch $
    applicative
      (undefined :: Two (Sum Int, String, Product Int) (Int, Int, String))
