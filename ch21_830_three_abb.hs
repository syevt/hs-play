import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend (Three' a b c) (Three' d e f) =
    Three' (mappend a d) (mappend b e) (mappend c f)

instance (Monoid a, Monoid b) => Semigroup (Three' a b) where
  (<>) = mappend

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (Three' a f f') <*> (Three' a' v v') = Three' (a <> a') (f v) (f' v')

instance Foldable (Three' a) where
  foldr f acc (Three' _ b c) = f b (f c acc)
  -- that's right: both `b` and `c` should take part in folding
  -- like they both are `fmap`ped in the Functor instance
  -- so 1st we need to to apply `f` to `c` and `acc` and this result
  -- should be passed to `f b acc` being the new value for `acc`

instance Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> f b <*> f c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ monoid
    (Three' (undefined :: Sum Int)
            (undefined :: Product Int)
            (undefined :: Product Int)
    )
  quickBatch
    $ functor (undefined :: Three' (Int, String, String) (String, Int, Int))
  quickBatch $ applicative
    (undefined :: Three' (Sum Int, String, Product Int) (Int, Int, String))
  quickBatch $ foldable
    (undefined :: Three' () (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))
  quickBatch $ traversable
    (undefined :: Three' () (Sum Int, Sum Int, Sum Int, Sum Int))
