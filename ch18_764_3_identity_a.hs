import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a =
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Arbitrary a, Monoid a) => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

trigger :: Identity (Sum Int, String, Product Int)
trigger = undefined

main :: IO ()
main = do
  quickBatch $ monoid (Identity "foo")
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
