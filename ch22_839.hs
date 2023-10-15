{-# LANGUAGE TupleSections #-}

import           Control.Applicative
import           Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) rev cap

tupledDo :: [Char] -> ([Char], [Char])
tupledDo s = do
  x <- return (rev s)
  y <- return (cap s)
  (x, y)

tupledDo' :: [Char] -> ([Char], [Char])
tupledDo' s = do
  let x = rev s
  let y = cap s
  (x, y)

-- tupledDo'' :: [Char] -> ([Char], [Char])
-- tupledDo'' s = do
  -- x <- (rev s, )
  -- y <- (cap s, )
  -- return (x, y)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind s = return (rev s) >>= \x -> return (cap s) >>= \y -> (x, y)
-- [hlint] Monad law, left identity
-- Found:
--   return (rev s) >>= \ x -> return (cap s) >>= \ y -> (x, y)
-- Why not:
--   (\ x -> return (cap s) >>= \ y -> (x, y)) (rev s)

tupledBind' :: [Char] -> ([Char], [Char])
tupledBind' s = (\x -> return (cap s) >>= \y -> (x, y)) (rev s)
-- [hlint] Found:
  -- \ y -> (x, y)
-- Why not:
  -- (x,)

tupledBind'' :: [Char] -> ([Char], [Char])
tupledBind'' s = (\x -> return (cap s) >>= (x, )) (rev s)
-- [hlint] Monad law, left identity
-- Found:
  -- return (cap s) >>= (x,)
-- Why not:
  -- (x,) (cap s)

tupledBind''' :: [Char] -> ([Char], [Char])
tupledBind''' s = (\x -> (x, ) (cap s)) (rev s) -- 🤣 🤣 🤣
-- interestingly enough `hlint` stopped here :) I saw nothing like
-- why not: (rev s, cap s)
