import           Control.Applicative
import qualified Data.Monoid                   as M
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data (,) a b = (,) a b
  deriving (Eq, Ord, Show)

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

instance Foldable ((,) a) where
  foldMap f (_, y) = f y
  foldr f z (_, y) = f y z

instance Traversable (Main.Either a) where
  traverse _ (Main.Left  x) = pure (Main.Left x)
  traverse f (Main.Right y) = Main.Right <$> f y

instance  (Eq a, Eq b) => EqProp ((,) a b) where
  (=-=) = eq

instance  (Arbitrary a, Arbitrary b) => Arbitrary ((,) a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ (,) a b

typeToCheck
  :: (,)
       ([M.Product Int], [M.Sum Int], [String])
       (M.Sum Int, String, M.Product Int)
typeToCheck = undefined


typeToCheckTraversable
  :: Main.Either
       ([M.Product Int], [M.Product Int], [M.Sum Int], [String])
       (M.Sum Int, M.Sum Int, String, M.Product Int)
typeToCheckTraversable = undefined

-- typeToCheckTraversable
  -- :: Main.Either
       -- (String, String, String, String)
       -- (String, String, String, String)
-- typeToCheckTraversable = undefined

main :: IO ()
main = do
  quickBatch $ functor typeToCheck
  quickBatch $ applicative typeToCheck
  quickBatch $ monad typeToCheck
  quickBatch $ traversable typeToCheckTraversable
