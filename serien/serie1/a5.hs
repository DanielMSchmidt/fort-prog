reversed :: [a] -> [a]
reversed [x] = [x]
reversed (x:xs) = (reversed xs) ++ [x]

-- reversed' :: [a] -> [a]
-- reversed' [x] = [x]
-- reversed' (a:bs:c) = [c] ++ (reversed' bs) ++ [a]

indexOf :: a -> [a] -> Maybe Int
indexOf needle [] = Nothing
indexOf needle (x:xs) = indexOf' 0 needle (x:xs)
  where indexOf' count needle (x:xs) = if needle == x
                                       then count
                                       else indexOf' (count + 1) needle xs
