split :: (a -> Bool) -> [a] -> ([a], [a])
split p [] = ([],[])
split p (x:xs) =
  let split' p [] ([trues],[falses]) = ([trues],[falses])
      split' p (x:xs) ([trues],[falses]) = if (p x)
                                           then split' p xs ([x] ++ [trues],[falses])
                                           else split' p xs ([trues],[x] ++ [falses])
  in split' p (x:xs) ([],[])
