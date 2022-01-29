j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f xs = xs >>= return . f

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f xs ys = xs >>= return . f >>= \x -> ys >>= \y -> return (x y)

l2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2' f xs ys = do
  x <- xs >>= return . f
  y <- ys
  return (x y)

l2'' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2'' f xs ys = do
  x <- f <$> xs
  y <- ys
  return (x y)

l2''' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2''' f xs ys = do
  fs <- return (f <$> xs)
  x <- fs
  y <- ys
  return (x y)

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap f x = do
  f' <- f
  x' <- x
  return (f' x')

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' f x = f >>= \f' -> x >>= \x' -> return (f' x')

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

lengthToMaybe :: String -> Maybe Int
lengthToMaybe x =
  if even l
    then Just l
    else Nothing
  where
    l = length x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (head:tail) f = l2 (:) (f head) (meh tail f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
