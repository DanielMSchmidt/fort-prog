reversed :: [a] -> [a]
reversed [x]    = [x]
reversed (x:xs) = (reversed xs) ++ [x]

-- reversed' :: [a] -> [a]
-- reversed' [x] = [x]
-- reversed' (a:bs:c) = [c] ++ (reversed' bs) ++ [a]

indexOf :: Int -> [Int] -> Maybe Int
indexOf needle []     = Nothing
indexOf needle (x:xs) = indexOf' 0
 where
  indexOf' count = if needle == x
                   then Just count
                   else indexOf' (count + 1)

concInter :: String -> [String] -> String
concInter _         []     = ""
concInter _         [x]    = x
concInter delimiter (x:xs) = x ++ delimiter ++ concInter delimiter xs
