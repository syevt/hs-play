import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four a b c d =
  Four a b c d
  deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend (Four a b c d) (Four e f g h) =
    Four (mappend a e) (mappend b f) (mappend c g) (mappend d h)

instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
         Semigroup (Four a b c d) where
  (<>) = mappend

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure a = Four mempty mempty mempty a
  (Four x y z f) <*> (Four x' y' z' v) =
    Four (x <> x') (y <> y') (z <> z') (f v)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $
    monoid
      (Four
         (undefined :: Sum Int)
         (undefined :: String)
         (undefined :: Product Int)
         (undefined :: String))
  quickBatch $
    functor
      (undefined :: Four (Int, String, Int) (Int, Int, String) ( String
                                                               , Int
                                                               , Int) ( Int
                                                                      , Int
                                                                      , Int))
  quickBatch $
    applicative
      (undefined :: Four (Sum Int, String, Product Int) ( String
                                                        , Product Int
                                                        , Sum Int) ( Sum Int
                                                                   , Product Int
                                                                   , String) ( Int
                                                                             , Int
                                                                             , String))
