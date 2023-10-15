boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

-- this type declaration will only work if `boop` and `doop` have the same type
-- explicitly set
bip :: Num a => a -> a
bip = boop . doop

boop' = (*2)
doop' = (+10)

bip' :: Integer -> Integer
-- the type definition may be skipped: `bip'` infers the type from `boop` and
-- `doop`
bip' = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop
