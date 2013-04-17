ggT :: Integer -> Integer -> Integer
ggT a 0 = a
ggT a b = ggT b (a `mod` b)

kgV :: Integer -> Integer -> Integer
kgV a b = (div (a * b) (ggT a b))


ggTL :: [Integer] -> Integer
ggTL [x] = x
ggTL (x:xs) = ggT x (ggTL xs)

kgVL :: [Integer] -> Integer
kgVL [x] = x
kgVL (x:xs) = kgV x (kgVL xs)