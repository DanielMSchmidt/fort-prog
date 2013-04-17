doubleMe x = x + x

doubleIfSmallerThen x y = if x < y
                            then x * 2
                            else x

-- Lists

a = [1,2,3,4,5] ++ [6,7,8]
b = 0:a

eins = head a

-- null [] is true, null [0,2] is false


lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"