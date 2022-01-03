import Control.Applicative
import Data.Semigroup
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil a = a
  mappend a Nil = a
  mappend (Cons x xs) ys = Cons x $ mappend xs ys

instance Semigroup (List a) where
  (<>) = mappend

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> values@(Cons v vs) = Cons (f v) (f <$> vs) <> (fs <*> values)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  (=-=) = eq

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Monoid a => Monoid (ZipList' a) where
  mempty = pure mempty
  mappend = liftA2 mappend
  -- mempty = ZipList' Nil
  -- mappend (ZipList' Nil) a = a
  -- mappend a (ZipList' Nil) = a
  -- mappend as bs = ZipList' (Cons (x <> y) vs)
    -- where
      -- (ZipList' (Cons x xs)) = as
      -- (ZipList' (Cons y ys)) = bs
      -- (ZipList' vs) = mappend (ZipList' xs) (ZipList' ys)

instance Monoid a => Semigroup (ZipList' a) where
  (<>) = mappend

instance Functor ZipList' where
  fmap _ (ZipList' Nil) = ZipList' Nil
  fmap f (ZipList' (Cons a la)) = ZipList' $ Cons (f a) (fmap f la)

instance Applicative ZipList' where
  pure a = ZipList' $ Cons a Nil
  _ <*> (ZipList' Nil) = ZipList' Nil
  (ZipList' Nil) <*> _ = ZipList' Nil
  (ZipList' (Cons f Nil)) <*> (ZipList' values) = ZipList' $ f <$> values
  (ZipList' (Cons f fs)) <*> values@(ZipList' (Cons v Nil)) =
    ZipList' $ Cons (f v) ls
    where
      (ZipList' ls) = ZipList' fs <*> values
  functions <*> values = ZipList' $ Cons (f v) ls
    where
      (ZipList' (Cons f fs)) = functions
      (ZipList' (Cons v vs)) = values
      (ZipList' ls) = ZipList' fs <*> ZipList' vs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a (Cons a Nil))]

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    frequency
      [(1, return (ZipList' Nil)), (3, return $ ZipList' (Cons a (Cons a Nil)))]

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: ZipList' (String, String, String))
  quickBatch $ monoid $ ZipList' (Cons "foo" (Cons "bar" Nil))
  quickBatch $ functor (undefined :: ZipList' (String, String, String))
  quickBatch $ applicative (undefined :: List (String, String, String))
  quickBatch $ monoid (Cons "foo" (Cons "bar" Nil))
  quickBatch $ functor (undefined :: List (String, String, String))
