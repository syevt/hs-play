data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk  a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a
