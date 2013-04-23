module A2 where
  data IntTree = Leaf Int | Branch Int IntTree IntTree | EmptyTree

  -- sumTree: sums each node of an int tree
  sumTree :: IntTree -> Int
  sumTree EmptyTree                = 0
  sumTree (Leaf a)                 = a
  sumTree (Branch root left right) = root + (sumTree left) + (sumTree right)

  -- mirrorTree: mirrors a tree vertically
  mirrorTree :: IntTree -> IntTree
  mirrorTree EmptyTree                = EmptyTree
  mirrorTree (Leaf a)                 = (Leaf a)
  mirrorTree (Branch root left right) = (Branch root right left)

  -- rtleri: concats a tree in the schema root, left subtree, right subtree
  rtleri :: IntTree -> [Int]
  rtleri EmptyTree                = []
  rtleri (Leaf a)                 = [a]
  rtleri (Branch root left right) = [root] ++ (rtleri left) ++ (rtleri right)

  -- rtrile: concats a tree in the schema root, right subtree, left subtree
  rtrile :: IntTree -> [Int]
  rtrile EmptyTree                = []
  rtrile (Leaf a)                 = [a]
  rtrile (Branch root left right) = [root] ++ (rtrile right) ++ (rtrile left)

  -- lertri: concats a tree in the schema  left subtree, root, right subtree
  lertri :: IntTree -> [Int]
  lertri EmptyTree                = []
  lertri (Leaf a)                 = [a]
  lertri (Branch root left right) = (lertri left) ++ [root] ++ (lertri right)

  -- rirtle: concats a tree in the schema right subtree, root, left subtree
  rirtle :: IntTree -> [Int]
  rirtle EmptyTree                = []
  rirtle (Leaf a)                 = [a]
  rirtle (Branch root left right) = (rirtle right) ++ [root] ++ (rirtle left)

  -- lerirt: concats a tree in the schema left subtree, right subtree, root
  lerirt :: IntTree -> [Int]
  lerirt EmptyTree                = []
  lerirt (Leaf a)                 = [a]
  lerirt (Branch root left right) = (lerirt left) ++ (lerirt right)  ++ [root]

  -- rilert: concats a tree in the schema right subtree, left subtree, root
  rilert :: IntTree -> [Int]
  rilert EmptyTree                = []
  rilert (Leaf a)                 = [a]
  rilert (Branch root left right) = (rilert right)  ++ (rilert left) ++ [root]

  -- Die Laufzeit aller Implementierungen ist O(n^2), da jede neue Ebene des Baums komplett durchlaufen werden muss