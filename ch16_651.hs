data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First  a) = First a
  fmap f (Second b) = Second (f b)

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v
