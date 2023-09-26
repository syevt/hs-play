class (Functor t, Foldable t) => Traversable t where

  {-# MINIMAL traverse | sequenceA #-}
  -- | Map each element of a structure to an action,
  -- evaluate these actions from left to right, and
  -- collect the results. For a version that ignores
  -- the results see 'Data.Foldable.traverse_'.
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f
  -- | Evaluate each action in the structure from
  -- left to right, and collect the results.
  -- For a version that ignores the results see
  -- 'Data.Foldable.sequenceA_'.
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
