combinator :: Monad t => Integral a => t a -> t a -> t a -> t (a, a, a)
combinator as bs cs = do
  a <- as
  b <- bs
  c <- cs
  return $ if even a then (100, b, c) else (a, b, c)
