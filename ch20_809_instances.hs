{-# LANGUAGE FlexibleInstances #-}

data Constant a b =
  Constant a
  deriving (Show)

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Foldable (Flip Constant a) where
  foldr f z (Flip (Constant a)) = f a z
  foldl f z (Flip (Constant a)) = f z a
  foldMap f (Flip (Constant a)) = f a

data Two a b =
  Two a b
  deriving (Show)

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldl f z (Two _ b) = f z b
  foldMap f (Two _ b) = f b

data Three a b c =
  Three a b c
  deriving (Show)

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldl f z (Three _ _ c) = f z c
  foldMap f (Three _ _ c) = f c

data Three' a b =
  Three' a b b
  deriving (Show)

instance Foldable (Three' a) where
  foldr f z (Three' _ _ c) = f c z
  foldl f z (Three' _ _ c) = f z c
  foldMap f (Three' _ _ c) = f c

data Four' a b =
  Four' a b b b
  deriving (Show)

instance Foldable (Four' a) where
  foldr f z (Four' _ _ _ c) = f c z
  foldl f z (Four' _ _ _ c) = f z c
  foldMap f (Four' _ _ _ c) = f c
