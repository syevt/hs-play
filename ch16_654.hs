newtype Wrap f a = Wrap (f a) deriving (Eq, Show)

-- instance Functor (Wrap f) where
  -- fmap f (Wrap fa) = Wrap (f fa)

-- instance Functor (Wrap f) where
  -- fmap f (Wrap fa) = Wrap (fmap f fa)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)
