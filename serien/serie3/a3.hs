foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ e [] = e
foldl' f e (x:xs) = foldl' f (f e x) xs


-- foldr (:) [] => Identität
-- foldl (:) []
-- foldr (-) 1 => Bei grader Anzahl alternierende Summe +1, sonst -1
-- foldl (-) 1 =>