import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList []
  -- mempty = pure mempty
  -- mappend = liftA2 mappend
  -- mappend x y = fmap mappend x <*> y
  mappend x y = mappend <$> x <*> y

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq
