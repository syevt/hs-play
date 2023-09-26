data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf a           ) = Leaf (f a)
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

instance Foldable Tree where
  foldr f b (Leaf a           ) = f a b
  foldr f b (Node a left right) = foldr f (foldr f (f a b) left) right

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node a left right) =
    Node <$> f a <*> traverse f left <*> traverse f right

-- instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  -- traverse f (Node x left right) = do
    -- left'  <- traverse f left
    -- right' <- traverse f right
    -- pure $ Node (f x) left' right'

-- Function to traverse a tree in preorder order
-- preorder' :: Node a a a -> Tree a
-- preorder' (Node x left right) = Node x (preorder' left) (preorder' right)

-- Function to traverse a tree in postorder order
-- postorder' :: Node a a a -> Tree a
-- postorder' (Node x left right) = postorder' right >> postorder' left >> pure x

-- Function to traverse a tree in inorder order
-- inorder' :: Node a a a -> Tree a
-- inorder' (Node x left right) = inorder' left >> pure x >> inorder' right

tree = Node 1 (Node 2 (Node 3 (Leaf 4) (Leaf 5)) (Leaf 6)) (Leaf 7)

--             1
--           /   \
--          2     7
--        /  \
--       3    6
--     /  \
--    4    5

-- This was an attepmt to understand how `traverse` unlike `fmap` can traverse a
-- structure in the particular order. It's not the case at all.
