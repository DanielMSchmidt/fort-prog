-- inits gibt eine Liste mit allen Anfangsstücken der Liste zurück
inits :: [a] -> [[a]]
inits [] = []
inits xs = inits' xs [] [[]]
  where 
    inits' [] ys zs     = zs
    inits' (x:xs) ys zs = inits' xs (ys++[x]) (zs++[ys++[x]])
	
-- tails gibt eine Liste mit allen Endstücken der Liste zurück	
tails :: [a] -> [[a]]
tails [] = []
tails xs = tails' xs [xs]
  where 
    tails' [] ys     = ys
    tails' (x:xs) ys = tails' xs (ys++[xs])

-- isPrefixOf überprüft ob der erste String Prefix des Zweiten ist.	
isPrefixOf :: String -> String -> Bool
isPrefixOf [] ys         = True
isPrefixOf xs []         = False
isPrefixOf (x:xs) (y:ys) = if (x==y)
                           then isPrefixOf xs ys
						   else False

-- isSuffixOf überprüft ob der erste String Suffix des Zweiten ist.							   
isSuffixOf :: String -> String -> Bool
isSuffixOf xs ys = isSuffixOf' (reverse xs) (reverse ys)
  where
    isSuffixOf' [] _          = True
    isSuffixOf' _ []          = False
    isSuffixOf' (x:xs) (y:ys) = if (x==y)
                                then isSuffixOf' xs ys
						        else False

-- insert fügt ein Element an jeder Stelle der Liste ein und liefert eine Liste von Listen mit allen Möglichkeiten.
insert :: a -> [a] -> [[a]]
insert x [] = [[x]]
insert x xs = insert' [] xs [] 
  where
    insert' gs [] js     = js++[gs++[x]]
    insert' [] (h:hs) js = insert' [h] hs (js++[[x]++(h:hs)])
    insert' gs (h:hs) js = insert' (gs++[h]) hs (js++[gs++[x]++(h:hs)])

-- perms berechnet alle Permutationen einer Liste	
perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = perms' xs []
  where
    perms' [] ys     = ys
    perms' (x:xs) [] = perms' xs [[x]]
    perms' (x:xs) ys = perms' xs (concat(map (insert x) ys))

-- split wendet eine boolesche Funktion auf jedes Element der Liste an. Alle Elemente welche Wahr ergeben kommen in eine Liste, alle anderen in eine andere Liste. Aus diesen beiden Listen wird ein Tupel gebildet und zurückgegeben.
split :: (a -> Bool) -> [a] -> ([a], [a])
split p [] = ([],[])
split p xs = split' xs [] []
  where
    split' [] ys zs     = (ys,zs)
    split' (x:xs) ys zs = if (p x)
	                      then split' xs (ys++[x]) zs
						  else split' xs ys (zs++[x])

-- quicksort wendet den Quicksort Algorithmus mit Hilfe der split Funktion auf eine Liste an.						  
quicksort :: [Int] -> [Int]
quicksort []     = []
quicksort (x:xs) = 
                let 
				  a     = split (<= x) xs
				  qsort = (quicksort(fst a)) ++ [x] ++ (quicksort(snd a))
                in qsort
