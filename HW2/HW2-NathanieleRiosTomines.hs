

--esercizio 1
--con genListList prendo una lista e ogni elemento è racchiusa in una lista annidata (di len 1)

genListList :: [a] -> [[a]]
genListList = foldr (\ x -> (++) [[x]]) []

--dato due liste ordinate, ritorna una lista che ha gli elementi di tutte e due le liste in maniera ordinata
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

--dato una lista di liste fa il merge con il primo e secondo, così via finché non ci si ritrova con un solo elemento 
mergeSortAux :: Ord a1 => [[a1]] -> [[a1]]
mergeSortAux [] = []
mergeSortAux (x:xs)
    | length (x:xs) == 1 = x:xs
    | otherwise = merge x (head xs) : mergeSortAux (tail xs)

--Simulo un "while len == 1" e ad ogni "iterazione" faccio chiamo il mergeSortAux (che a sua volta prende la lista e fa il merge a due a due della lista di liste)
mergeSortAux2 :: Ord a1 => [[a1]] -> [[a1]]
mergeSortAux2 [] = []
mergeSortAux2 xs
    | length xs == 1 = xs
    | otherwise =(mergeSortAux2 . mergeSortAux) xs

--pensandola sequenzialmente quello che faccio nella mergeSort è: 
--1) generare la lista di liste con genListList 
--2)fare il mergesort su tutta la lista finchè ho un solo risultato 
--3) estrapolare l'unico risultato che ho con head
mergeSort :: Ord a1 => [a1] -> [a1]
mergeSort = head . mergeSortAux2 . genListList

{--
    Complessità:
    L'algoritmo è la versione Bottom-Up del merge sort.
    Il merge ha complessità O(n) e viene eseguita O(log n) volte, quindi la complessità dell'algoritmo
    corrisponde a O(n log n)
--}


--Esercizio 2
--def di bintree
data BinTree a = R (BinTree a) a (BinTree a) | F a deriving (Show)

foldrBT :: (t1 -> t2 -> t2) -> t2 -> BinTree t1 -> t2
foldrBT f y (F a) = f a y
foldrBT f y (R left a right) = f a (foldrBT f (foldrBT f y right) left)


mapBT :: (t -> a) -> BinTree t -> BinTree a
mapBT f (F a) = F (f a)
mapBT f (R left a right) = R (mapBT f left) (f a) (mapBT f right)

--Funzione per trasformare un albero con nodi di valore x e foglie di valore y
transBT :: BinTree a -> b -> b -> BinTree b
transBT (F a) _ y = F y
transBT (R left a right) x y = R (transBT left x y) x (transBT right x y)

--Es 2.1
--Conteggio dei nodi che non sono foglie
nNodesStrict :: BinTree a -> Integer
nNodesStrict bt = foldrBT (\x -> (+x)) 0 (transBT bt 1 0)

--Conteggio di tutti i nodi (anche quelle foglia)
nNodes :: BinTree a -> Integer
nNodes = foldrBT (const (+ 1)) 0

--Es 2.2
--calcolo profondità
profBTAux :: Num a1 => BinTree a2 -> a1 -> BinTree a1
profBTAux (F a) p = F p
profBTAux (R left a right) p = R (profBTAux left (p + 1) ) p (profBTAux right (p + 1))

profBT :: Num a1 => BinTree a2 -> BinTree a1
profBT bt= profBTAux bt 0

--Calcolo altezza
hBT :: (Ord a, Num a) => BinTree a2 -> a
hBT bt= foldrBT (\v i -> if i>v then i else v) 0 (profBT bt)

--Es 2.3
treeOfSb :: (Ord a, Num a) => BinTree a2 -> BinTree a
treeOfSb (F a) = F 0
treeOfSb (R left a right) = R (treeOfSb left) (abs (hBT left - hBT right)) (treeOfSb right)

--Calcolo massimo sbilanciamento
maxSB :: (Ord a, Num a) => BinTree a2 -> a
maxSB bt = foldrBT (\x i -> if x>i then x else i) 0 (treeOfSb bt)

--(R (F 3) 3 (R (R (R (R (F 3) 3 (F 3)) 3 (F 3)) 3 (F 3)) 3 (F 3)))

--Es 3
--Def Tree
data Tree a =N a [Tree a] deriving (Show)


foldrT :: (a -> b -> b) -> b -> Tree a -> b
foldrT f i (N a []) = f a i
foldrT f i (N a ts) = f a (foldr (\x ris -> foldrT f ris x) i ts)

mapT :: (t -> a) -> Tree t -> Tree a
mapT f (N a []) = N (f a) []
mapT f (N a ts) = N (f a) (map (mapT f) ts)

--Es 4
--esempio del cammino più lungo che non passa per la radice
-- R (R (R (R (F 10) 6 (F 11)) 4 (F 7) ) 2 (R (F 8) 5 (R (F 12) 9 (F 13)) )) 1 (F 3)

--L'idea è quello di salvarsi diametro e altezza per ogni ricorsione, così da poter confrontare il diametro del nodo (determinato da h left + h right + 2 ) con i rispettivi figli destro e sinistro, mentre l'altezza è data dal massimo (h left , h right) + 1
diametroBTAux :: (Ord a1, Num a1) => BinTree a2 -> (a1, a1) -> (a1, a1)
diametroBTAux (F _) (d,h) = (d,h)
diametroBTAux (R left _ right) (d,h) = ( max dRoot (max dL dR), altezza )
    where
        dRoot = snd (diametroBTAux left (d,h)) + snd (diametroBTAux right (d,h)) + 2
        dL = fst (diametroBTAux left (d,h))
        dR = fst (diametroBTAux right (d,h))
        altezza = max (snd (diametroBTAux left (d,h))) (snd (diametroBTAux right (d,h))) +1


--Calcola il diametro (massimo numero dei nodi che percorre ) di un albero binario
diametroBT :: (Ord b, Num b) => BinTree a2 -> b
diametroBT bt =fst (diametroBTAux bt (0,0))

--Es 5
{--
    scanr f e = map (foldr f e) . tails

    Caso base: []
        scanr f e [] = 
            {def scanr } map (foldr f e ) . tails [] =
            {def .} map (foldr f e) (tails []) =
            {def tails} map (foldr f e ) [[]] =
            {def map} [foldr f e []] =
            {def foldr} [e]

    Caso base: scanr f e [] = [e]
--}

{--

    Passo Ricorsivo (x:xs)
    scanr f e (x:xs) = 
        {def scanr e .}                                 map (foldr f e) (tails (x:xs)) = 
        {def tails}                                     map (foldr f e ) ((x:xs) : tails (xs)) = 
        {def di map}                                    foldr f e (x:xs) : map (foldr f e) (tails xs)=
        {def di . e scanr}                              foldr f e (x:xs) : scanr f e xs=
        {def di foldr}                                  f x (foldr f e xs) : scanr f e xs=
        {dim foldr f e xs = head (scanr f e xs) (**) }  f x ( head (scanr f e xs)) : scanr f e xs=
        {uso di where}                                  f x (head scanXs) : scanXs where scanXs = scanr f e xs

        (**)
        head (scanr f e xs) = 
            {def scanr e .} head (map (foldr f e) . tails xs)=
            {def tails}     head (map (foldr f e ) (xs : tails (tail xs)) )=
            {def map}       head (foldr f e xs : map (foldr f e ) (tails (tails xs)))=
            {def foldr}     foldr f e xs
            
            

    Caso Ricorsivo: 
    scanr f e (x:xs) = f x (head scanXs) : scanXs where scanXs = scanr f e xs
        
--}

myScanr :: (t -> a -> a) -> a -> [t] -> [a]
myScanr f e [] = [e]
myScanr f e (x:xs)= f x (head scanXs) : scanXs where scanXs = scanr f e xs