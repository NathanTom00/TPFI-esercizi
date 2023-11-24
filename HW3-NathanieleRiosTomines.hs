--es 1
insonnia :: [Char]
insonnia = nextInsonnia 0 where nextInsonnia x = show x ++ " sheep " ++ nextInsonnia (x+1)

--Es 2
tart :: Integer -> [Integer]
tart 0 = [1]
tart 1 = [1, 1]
tart n = let p = tart (n - 1)
    in [1] ++ tartAux p ++ [1]

tartAux :: [Integer] -> [Integer]
tartAux [] = []
tartAux [_] = []
tartAux (x:y:xs) = x + y : tartAux (y : xs)

tartaglia :: [[Integer]]
tartaglia = [tart x | x  <-[0..]]

--Es 3
luckyNum :: [Integer]
luckyNum = 1: filterLucky (zip [el | el <- [3,5..]] [i | i <- [2..]]) 2
    where 
        filterLucky (x:xs) nextI = fst x : filterLucky (zip [fst el' | el' <- xs , mod (snd el') (fst x)  /= 0]  [i' | i' <-[nextI+1..]]) (nextI+1)


--Es 4
allSums :: Num b => [b] -> [[b]]
allSums (x:xs) = map (x+) xs : allSums xs
        


{--
    allSums ulams attendeva all'infinito di stampare gli ulams e quindi contaOcc prendeva una sequenza infinita di numeri non terminando mai.
    Una soluzione che ho trovato è quello di salvarsi ogni volta i numeri di ulams con un parametro (us) e usare allSums su una lista finita.
--}

{--
contaOcc :: (Eq t, Num p) => [t] -> t -> p
contaOcc [] _ = 0
contaOcc (x:xs) n = if x == n then 1 + contaOcc xs n else contaOcc xs n

ulams :: [Integer]
ulams = 1:2: ulamsAux (allSums [1,2]) 3 [1,2]
        where ulamsAux xss n us= if contaOcc (concat xss) n == 1 then n : ulamsAux (allSums (us++[n])) (n+1) (us++[n]) else ulamsAux (allSums us) (n+1) us

--}



{--
    Una soluzione alternativa è quella di gestire la lista infinita prodotta da AllSums ulams attraverso contaOcc.
--}
ulams :: [Integer]
ulams = 1:2: ulamsAux (allSums ulams) 3 2
        where
            ulamsAux xss n n_ulams = if contaOcc xss n 0 0 (n_ulams-2) (n_ulams-1)  == 1 then n : ulamsAux (allSums ulams) (n+1) (n_ulams+1) else ulamsAux (allSums ulams) (n+1) n_ulams

{--
    In questo caso contaOcc avrà due indici x e y con due limiti:
        *Se y supera la lunghezza consentita allora ho finito di contare
        *se x supera la sua lunghezza consentita allora devo partire dal primo elemento della riga successiva (x=0 y=y+1)
        *se x = 0 e n è più piccolo dell'elemento che stiamo controllando con gli indici x e y allora significa che tutti gli altri numeri sia in orizzontale che in verticale successivi sono più grandi di n (termino di contare)
        *se l'elemento che stiamo puntando = n allora lo conto (1) e continuo a contare dalla nuova riga
        *altrimenti continuo a leggere normalmente (ovvero incrementando la x) la riga
--}
contaOcc :: (Num a1, Ord a2) => [[a2]] -> a2 -> Int -> Int -> Int -> Int -> a1
contaOcc xss n x y len_x len_y
    |y > len_y = 0
    |x > len_x = contaOcc xss n 0 (y+1) (len_x-1) len_y
    |x == 0 && n < xss !! y !! x = 0
    |xss !! y !! x == n = 1 + contaOcc xss n 0 (y+1) (len_x-1) len_y
    |otherwise = contaOcc xss n (x+1) y len_x len_y