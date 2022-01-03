import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a =
  Pair a a
  deriving (Eq, Show)

instance Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend (Pair a b) (Pair c d) = Pair (mappend a c) (mappend b d)

instance Monoid a => Semigroup (Pair a) where
  (<>) = mappend

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid (Pair "foo" "bar")
  quickBatch $ monoid (Pair (undefined :: Sum Int) (undefined :: Sum Int))
  quickBatch $
    functor
      (undefined :: Pair ( (Product Int, Product Int)
                         , (Product Int, Product Int)
                         , (Product Int, Product Int)))
  quickBatch $
    applicative
      (undefined :: Pair ((String, String), (String, String), (String, String)))
