import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend (Three a b c) (Three d e f) =
    Three (mappend a d) (mappend b e) (mappend c f)

instance (Monoid a, Monoid b, Monoid c) => Semigroup (Three a b c) where
  (<>) = mappend

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (Three a b f) <*> (Three c d v) = Three (a <> c) (b <> d) (f v)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $
    monoid
      (Three
         (undefined :: Sum Int)
         (undefined :: String)
         (undefined :: Product Int))
  quickBatch $
    functor
      (undefined :: Three (Int, String, Int) (Int, Int, String) ( String
                                                                , Int
                                                                , Int))
  quickBatch $
    applicative
      (undefined :: Three (Sum Int, String, Product Int) ( String
                                                         , Product Int
                                                         , Sum Int) ( Int
                                                                    , Int
                                                                    , String))
