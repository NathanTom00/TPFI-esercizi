
--Esercizio 1.1
shuffle :: Eq a => [a] -> [a] -> [a] -> Bool
shuffle [] [] [] = True;
shuffle [] [] _ = False;
shuffle [] _ [] = False;
shuffle _ [] [] = False;
shuffle [] ys zs
  | last ys == last zs = shuffle [] (init ys) (init zs)
  | otherwise  = False
shuffle xs [] zs
  | last xs == last zs = shuffle (init xs) [] (init zs)
  | otherwise  = False
shuffle xs ys zs
  | last xs == last zs = shuffle (init xs) ys (init zs)
  | last ys == last zs = shuffle xs (init ys) (init zs)
  | otherwise = False;

--Esercizio 1.2
shuffleAux :: Eq a => [[a]] -> [a] -> Bool
shuffleAux [] [] = False
shuffleAux [[]] [] = True
shuffleAux _ [] = False
shuffleAux [] _ = False
shuffleAux (x:xs) zs
  | x == zs = True
  | otherwise = shuffleAux xs zs;

shuffle2 :: Eq a => [a] -> [a] -> [a] -> Bool
shuffle2 xs ys zs = shuffleAux (genShuffle xs ys) zs

--Esercizio 1.3 lo shuffle dell'esercizio 1.1 dovrebbe ammettere anche i duplicati
shufflePlus :: Eq a => [a] -> [a] -> [a] -> Bool
shufflePlus [] [] [] = True;
shufflePlus [] [] _ = False;
shufflePlus [] _ [] = False;
shufflePlus _ [] [] = False;
shufflePlus [] ys zs
  | last ys == last zs = shufflePlus [] (init ys) (init zs)
  | otherwise  = False
shufflePlus xs [] zs
  | last xs == last zs = shufflePlus (init xs) [] (init zs)
  | otherwise  = False
shufflePlus xs ys zs
  | last xs == last zs = shufflePlus (init xs) ys (init zs)
  | last ys == last zs = shufflePlus xs (init ys) (init zs)
  | otherwise = False;

--Esercizio 1.4
genShuffle :: [a] -> [a] -> [[a]]
genShuffle [] [] = [[]]
genShuffle [] ys = [ys]
genShuffle xs [] = [xs]
genShuffle (x:xs) (y:ys) = map (x:) (genShuffle xs (y:ys)) ++ map (y:) (genShuffle (x:xs) ys)

--Esercizio 2
segmentsAux :: [a] -> [[a]]
segmentsAux [] = []
segmentsAux (x:xs) = (x:xs) : segmentsAux xs

segments :: [a] -> [[a]]
segments [] = [[]]
segments xs = segmentsAux xs ++ segments (init xs)

--Esercizio 3.1
applyL :: [t -> a] -> [t] -> [a]
applyL (f:fs) (x:xs)= f x : applyL fs xs
applyL _ _ = []

myZipWith :: (a1 -> t -> a2) -> [a1] -> [t] -> [a2]
myZipWith f xs ys = applyL (map f xs) ys

--Esercizio 3.2
myMap :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
myMap f xs = foldr op [] xs
  where op x risPrima = [f x] ++ risPrima



--Esercizio 3.3
myMap2 :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
myMap2 f xs = foldl op [] xs
  where op risPrima x= risPrima ++ [f x]

--Esercizio 3.4
-- map non può essere usata per creare myFoldr e myFoldl perché map ritorna sempre una lista, mentre foldr e foldl possono restituire tipi diversi da una lista

--Esercizio 4.1
sommaDivisori :: Integral a => a -> a
sommaDivisori n = sum [d | d <- [1..n], rem n d == 0]


part :: Integral p => p -> p
part 0 = 1
part 1 = 1
part n = div (sum [sommaDivisori (n-k) * part k | k <-[0..n-1]]) n