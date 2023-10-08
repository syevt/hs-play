import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty               = Empty
  fmap f (Leaf a           ) = Leaf (f a)
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldr _ b Empty               = b
  foldr f b (Leaf a           ) = f a b
  foldr f b (Node a left right) = foldr f (foldr f (f a b) left) right

instance Traversable Tree where
  traverse f Empty    = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node a left right) =
    Node <$> f a <*> traverse f left <*> traverse f right

tree = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5)) (Leaf 6)) (Leaf 7)

--             1
--           /   \
--          2     7
--        /  \
--       3    6
--     /  \
--    4    5

treeEven = Node 2 (Node 4 (Node 6 (Leaf 8) (Leaf 10)) (Leaf 12)) (Leaf 14)

f :: (Num a, Integral a) => a -> Maybe a
f x = if even x then Just (x * 2) else Nothing

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    elements
      [ Empty
      , Leaf a
      , Node a (Leaf a)                   (Leaf a)
      , Node a (Node a (Leaf a) (Leaf a)) (Leaf a)
      , Node a (Leaf a)                   (Node a (Leaf a) (Leaf a))
      , Node a (Leaf a) (Node a (Node a (Leaf a) (Leaf a)) (Leaf a))
      ]

triggerTwo :: Tree ([String], [String], [String], [String])
triggerTwo = undefined

triggerThree :: Tree (Sum Int, Product Int, String, Sum Int, Sum Int)
triggerThree = undefined

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Tree (String, String, String))
  quickBatch $ foldable triggerThree
  quickBatch $ traversable triggerTwo
