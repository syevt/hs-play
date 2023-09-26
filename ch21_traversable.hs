data Tree a = Con a | Add (Tree a) (Tree a)
        deriving (Eq, Show)

instance Functor Tree where
  fmap f (Con a  ) = Con (f a)
  fmap f (Add x y) = Add (fmap f x) (fmap f y)

instance Foldable Tree where
  foldr f b (Con a  ) = f a b
  foldr f b (Add x y) = foldr f (foldr f b x) y

instance Traversable Tree where
  traverse fn (Con a  ) = Con <$> fn a
  traverse fn (Add x y) = Add <$> traverse fn x <*> traverse fn y

t1 = Con 3                              -- 3
t2 = Add (Con 3) (Con 4)                -- 3 + 4
t3 = Add (Add (Con 3) (Con 4)) (Con 2)  -- (3 + 4) + 2

f x = if even x then Just (x `div` 2) else Nothing

g n = [ 1 .. n ]
