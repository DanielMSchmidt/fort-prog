data Tree a = Leaf a | Node (Tree a) (Tree a) | Empty deriving (Show)

-- Create an empty tree
empty :: Tree a
empty = Empty

-- Create a leaf
leaf :: a -> Tree a
leaf x = Leaf x

-- Create a tree with the given trees as branches
(&) :: Tree a -> Tree a -> Tree a
(&) l r = Node l r


-- Flattens the tree by one hierarchical layer
flatTree :: Tree (Tree a) -> Tree a
flatTree Empty      = Empty
flatTree (Leaf a)   = a
flatTree (Node l r) = (Node (flatTree l) (flatTree r))


-- Calls a function on each Element of the Tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty      = Empty
mapTree f (Leaf a)   =  Leaf (f a)
mapTree f (Node l r) = (Node (mapTree f l) (mapTree f r))


-- Folds a Tree
foldTree :: b -> (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree neutral _ _ Empty                 = neutral
foldTree _ function _ (Leaf a)             = (function a)
foldTree neutral function merge (Node l r) = (merge (foldTree neutral function merge l) (foldTree neutral function merge r))

-- Extends a Tree by a given one
extendTree :: (a -> Tree b) -> Tree a -> Tree b
extendTree _ Empty             = Empty
extendTree function (Leaf a)   = (function a)
extendTree function (Node l r) = (Node (extendTree function l) (extendTree function r))
