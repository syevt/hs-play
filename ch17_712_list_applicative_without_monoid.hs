import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> values@(Cons v vs) =
    Cons (f v) (f <$> vs) `append` (fs <*> values)

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a (Cons a Nil))]

main :: IO ()
main = do
  quickBatch $ functor (undefined :: List (String, String, String))
  quickBatch $ applicative (undefined :: List (String, String, String))
