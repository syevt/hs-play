-- import           Data.Monoid
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Checkers
-- import           Test.QuickCheck.Classes

data S n a = S (n a) a
  deriving (Eq, Show)
-- to make it easier, we'll give you the constraints.

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldr f acc (S na a) = f a (foldr f acc na)

instance (Traversable n) => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a
  -- `traverse` in `traverse f na` is not a recursive call!!!
  -- that's the `traverse` coming from the `Traversable` instance defined in `n`
  -- did not get why this
  -- traverse f (S na a) = S (fold (traverse f na)) <$> f a
  -- does not work 😭

f :: (Num a, Integral a) => a -> Maybe a
f x = if even x then Just (x * 2) else Nothing

-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-- S [16] 18
-- let f = if even x then Just (x*2) else Nothing
-- Just $ S [32] 36
-- fmap & traverse [Just 32] & Just 36 -> Just $ S [32] 36
-- traverse & traverse Just [32] & Just 36 -> Just $ S [32] 36

-- data Three' a b = Three' a b b
  -- deriving (Eq, Show)

-- instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  -- mempty = Three' mempty mempty mempty
  -- mappend (Three' a b c) (Three' d e f) =
    -- Three' (mappend a d) (mappend b e) (mappend c f)

-- instance (Monoid a, Monoid b) => Semigroup (Three' a b) where
  -- (<>) = mappend

-- instance (Arbitrary a) => Arbitrary (S n a) where
  -- arbitrary = do
    -- -- n <- elements [Just, Nothing]
    -- a <- arbitrary
    -- return $ S (Just a) a

-- instance Functor (Three' a) where
  -- fmap f (Three' a b c) = Three' a (f b) (f c)

-- instance Monoid a => Applicative (Three' a) where
  -- pure a = Three' mempty a a
  -- (Three' a f f') <*> (Three' a' v v') = Three' (a <> a') (f v) (f' v')

-- instance Foldable (Three' a) where
  -- foldr f acc (Three' _ b c) = f b (f c acc)
  -- -- that's right: both `b` and `c` should take part in folding
  -- -- like they both are `fmap`ped in the Functor instance
  -- -- so 1st we need to to apply `f` to `c` and `acc` and this result
  -- -- should be passed to `f b acc` being the new value for `acc`

-- instance Traversable (Three' a) where
  -- traverse f (Three' a b c) = Three' a <$> f b <*> f c

-- instance (Eq a) => EqProp (S n a) where
  -- (=-=) = eq

-- main :: IO ()
-- main = do
  -- quickBatch $ monoid
    -- (Three' (undefined :: Sum Int)
            -- (undefined :: Product Int)
            -- (undefined :: Product Int)
    -- )
  -- quickBatch $ functor (undefined :: S (Int, String, String) (String, Int, Int))
  -- quickBatch $ applicative
    -- (undefined :: Three' (Sum Int, String, Product Int) (Int, Int, String))
  -- quickBatch $ foldable
    -- (undefined :: Three' () (Sum Int, Sum Int, Sum Int, Sum Int, Sum Int))
  -- quickBatch $ traversable
    -- (undefined :: Three' () (Sum Int, Sum Int, Sum Int, Sum Int))
