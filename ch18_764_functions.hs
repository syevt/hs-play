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
