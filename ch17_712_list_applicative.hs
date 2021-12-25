-- import           Control.Applicative
-- import           Data.Monoid
import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil         a   = a
  mappend a           Nil = a
  mappend (Cons x xs) ys  = Cons x $ mappend xs ys

instance Semigroup (List a) where
  (<>) = mappend
  -- Nil         <> a   = a
  -- a           <> Nil = a
  -- (Cons x xs) <> ys  = Cons x $ xs <> ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure a = undefined
  (Cons f fl) <*> (Cons v vl) = Cons (f v) (fl <*> vl)

-- pure id <*> v = v

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a (Cons a Nil))]

main :: IO ()
main = do
  quickBatch $ monoid (Cons 2 (Cons 3 Nil))
  -- quickBatch $ functor (undefined :: List (String, String, String))
  -- quickBatch $ applicative (undefined :: List (String, String, String))
