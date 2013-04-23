-- Quicksort

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = qsort(filter (<= x) xs) ++ [x] ++ qsort(filter (> x) xs)