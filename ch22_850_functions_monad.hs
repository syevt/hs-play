foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+ 1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- froot does a couple of things
-- kinda both of what `foo` and `bar` do
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+ 1) r, length r)

-- but we can achieve the same using `foo` and `bar`
-- we only need to change `bar` to take a singe argument
barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

-- but that doesn not incrment the `r`'s memebers, so
barPlus r = (foo r, length r)

-- this also can be achieved by passing `foo r` and `r` intself to `bar`
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- or more `Reader`-y
frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r
