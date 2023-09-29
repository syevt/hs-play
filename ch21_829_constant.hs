import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance (Monoid a) => Monoid (Constant a b) where
  mempty = Constant mempty
  mappend (Constant a) (Constant b) = Constant (mappend a b)

instance (Monoid a) => Semigroup (Constant a b) where
  (<>) = mappend

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant a) <*> (Constant b) = Constant (a <> b)

instance Foldable (Constant a) where
  foldr _ b _ = b

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a, Monoid a) => Arbitrary (Constant a b) where
  arbitrary = do
    Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

trigger
  :: Constant (Sum Int, String, Product Int) (Sum Int, String, Product Int)
trigger = undefined

triggerTwo
  :: Constant
       ([String], [String], [String], [String])
       (String, String, String, String)
triggerTwo = undefined

main :: IO ()
main = do
  quickBatch $ monoid (Constant "foo")
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  -- quickBatch $ monad trigger
  quickBatch $ traversable triggerTwo
