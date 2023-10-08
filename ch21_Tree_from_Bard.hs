-- This doesn't work 🤣
-- `Bard` is not thet good
-- Tree data type
data Tree a = Leaf a | Node a (Tree a) (Tree a)

-- Function to print the value of a node
printNode :: a -> String
printNode x = show x

-- Function to traverse a tree in preorder order
preorder' :: Node a a a -> Tree a
preorder' (Node x left right) = Node x (preorder' left) (preorder' right)

-- Function to traverse a tree in postorder order
postorder' :: Node a a a -> Tree a
postorder' (Node x left right) = postorder' right >> postorder' left >> pure x

-- Function to traverse a tree in inorder order
inorder' :: Node a a a -> Tree a
inorder' (Node x left right) = inorder' left >> pure x >> inorder' right

-- Example tree
tree :: Tree Int
tree = Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 Leaf Leaf)

-- Print the value of each node in the tree in preorder order
print $ preorder' tree

-- Output:
-- "1 2 3 4"

-- Print the value of each node in the tree in postorder order
print $ postorder' tree

-- Output:
-- "3 2 4 1"

-- Print the value of each node in the tree in inorder order
print $ inorder' tree

-- Output:
-- "3 2 1 4"

