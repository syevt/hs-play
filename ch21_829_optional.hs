import           Data.Monoid
import           Data.Semigroup
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  Nada   <> Nada   = Nada
  Nada   <> Only x = Only x
  Only x <> Nada   = Only x
  Only x <> Only y = Only (x <> y)

instance Functor Optional where
  fmap _ Nada     = Nada
  fmap f (Only a) = Only (f a)

instance Applicative Optional where
  pure = Only
  _        <*> Nada     = Nada
  Nada     <*> _        = Nada
  (Only f) <*> (Only a) = Only (f a)

instance Monad Optional where
  return = pure
  Nada >>= _ = Nada
  (Only a) >>= f = f a

instance Foldable Optional where
  foldr _ b Nada     = b
  foldr f b (Only a) = f a b

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Only a) = Only <$> f a

instance  Eq a => EqProp (Optional a) where
  (=-=) = eq

instance  Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(2, return Nada), (2, return $ Only a)]

typeToCheck :: Optional (Sum Int, String, Product Int)
typeToCheck = undefined

typeToCheckFoldable
  :: Optional (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int)
typeToCheckFoldable = undefined

typeToCheckTraversable
  :: Optional (Sum Int, Sum Int, Sum Int, Sum Int)
typeToCheckTraversable = undefined

main :: IO ()
main = do
  quickBatch $ monoid (Only "foo")
  quickBatch $ functor typeToCheck
  quickBatch $ applicative typeToCheck
  quickBatch $ monad typeToCheck
  quickBatch $ foldable typeToCheckFoldable
  quickBatch $ traversable typeToCheckTraversable
