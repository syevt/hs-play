import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Functor t, Foldable t, Eq a) => a -> t a -> Bool
elem' e = getAny . foldMap Any . fmap (== e)

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' e = getAny . foldMap (Any . (== e))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where
    f a Nothing = Just a
    f a (Just b) = Just (min a b)

-- that's because
-- Prelude Data.Monoid Data.Foldable> foldr undefined Nothing []
-- Nothing
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where
    f a Nothing = Just a
    f a (Just b) = Just (max a b)

null' :: (Foldable t) => t a -> Bool
null' = foldr f True
  where
    f _ _ = False

null'' :: (Foldable t) => t a -> Bool
null'' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x acc -> x : acc) []

toList'' :: Foldable t => t a -> [a]
toList'' = foldr (:) []

-- fold :: (Foldable t, Monoid m) => t m -> m
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (<> mempty)

fold'' :: (Foldable t, Monoid m) => t m -> m
fold'' = foldMap id

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr ((<>) . f) mempty

foldMap''' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap''' f = foldr (\x y -> f x <> y) mempty
