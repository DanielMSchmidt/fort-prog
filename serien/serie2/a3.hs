import A2
type SearchTree = IntTree


-- insert: Adds number to SearchTree
insert :: Int -> SearchTree -> SearchTree
insert a EmptyTree                = (Leaf a)
insert a (Leaf b)                 = if a < b
                                    then (Branch b (Leaf a) EmptyTree)
                                    else (Branch a (Leaf b) EmptyTree)
insert a (Branch root left right) = if a < root
                                    then (Branch root (insert a left) right)
                                    else (Branch root left (insert a right))

-- isElem: Searches number in SearchTree
isElem :: Int -> SearchTree -> Bool
isElem a EmptyTree                = False
isElem a (Leaf b)                 = a == b
isElem a (Branch root left right)
  | a == root = True
  | a < root  = (isElem a left)
  | a > root  = (isElem a right)
  | otherwise = False


-- delete: Deletes number from SearchTree
delete :: Int -> SearchTree -> SearchTree
delete a EmptyTree                = EmptyTree
delete a (Leaf b)
  | a == b    = EmptyTree
  | otherwise = (Leaf b)
delete a (Branch root left right)
  | a == root  = (Branch (getBiggestSmaller left) (delete (getBiggestSmaller left) left) right)
  | a < root   = (delete a left)
  | otherwise  = (delete a right)
  where
    getBiggestSmaller (Leaf b)                      = b
    getBiggestSmaller (Branch root EmptyTree right) = (getBiggestSmaller right)
    getBiggestSmaller (Branch root left EmptyTree)  = (getBiggestSmaller left)
    getBiggestSmaller (Branch root left right)      = (getBiggestSmaller right)