import Control.Applicative
import qualified Data.Monoid as M
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap _ (First e) = First e
  fmap f (Second a) = Second (f a)

instance Monoid e => Applicative (Sum e) where
  pure a = Second a
  (First e) <*> (First _) = First e
  (First e) <*> (Second _) = First e
  (Second _) <*> (First e) = First e
  (Second f) <*> (Second a) = Second $ f a

instance Monoid a => Monad (Sum a) where
  return = pure
  (First e) >>= _ = First e
  (Second a) >>= f = f a

instance (Eq e, Eq a) => EqProp (Sum e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Sum e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [(1, return $ First e), (3, return $ Second a)]

typeToCheck ::
     Sum ([M.Product Int], [M.Sum Int], [String]) ( M.Sum Int
                                                  , String
                                                  , M.Product Int)
typeToCheck = undefined

main :: IO ()
main = do
  quickBatch $ functor typeToCheck
  quickBatch $ applicative typeToCheck
  quickBatch $ monad typeToCheck
