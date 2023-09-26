import           Control.Applicative
import qualified Data.Monoid                   as M
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Either a b = Left a | Right b deriving (Eq, Ord, Show)

instance Functor (Main.Either a) where
  fmap _ (Main.Left  x) = Main.Left x
  fmap f (Main.Right y) = Main.Right (f y)

instance Applicative (Main.Either e) where
  pure = Main.Right
  Main.Left  e <*> _ = Main.Left e
  Main.Right f <*> r = fmap f r

instance Foldable (Main.Either a) where
  foldMap _ (Main.Left  _) = mempty
  foldMap f (Main.Right y) = f y
  foldr _ z (Main.Left  _) = z
  foldr f z (Main.Right y) = f y z

instance Traversable (Main.Either a) where
  traverse _ (Main.Left  x) = pure (Main.Left x)
  traverse f (Main.Right y) = Main.Right <$> f y

instance Monoid a => Monad (Main.Either a) where
  return = pure
  (Main.Left a) >>= _ = Main.Left a
  (Main.Right b) >>= f = f b

instance (Eq e, Eq a) => EqProp (Main.Either e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Main.Either e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    frequency [(1, return $ Main.Right e), (3, return $ Main.Left a)]

typeToCheck
  :: Main.Either
       ([M.Product Int], [M.Sum Int], [String])
       (M.Sum Int, String, M.Product Int)
typeToCheck = undefined

typeToCheckTraversable
  :: Main.Either
       (String, String, String, String)
       (String, String, String, String)
typeToCheckTraversable = undefined

main :: IO ()
main = do
  quickBatch $ functor typeToCheck
  quickBatch $ applicative typeToCheck
  quickBatch $ monad typeToCheck
  quickBatch $ traversable typeToCheckTraversable
