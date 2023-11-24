--Es 1
getInt :: IO Int
getInt = do
    x <- getLine
    return(read x :: Int)


{--
adder :: IO ()
adder = add (getInt)
--}
adder :: IO()
adder = do
    putStrLn "Mettere n:"
    n <- getInt
    x <- sommaNumero n
    putStrLn "Risultato: "
    print x




sommaNumero :: Int -> IO Int
sommaNumero 0 = return 0
sommaNumero n =
        do
            putStrLn "Mettere un numero:"
            x <- getInt
            prox <-  sommaNumero (n-1)
            return (x + prox)


--Es 2
subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ (pure  (x:) <*> subsets xs)

divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

isSemiPerfect :: Integral a => a -> Bool
isSemiPerfect n
    | [x | x <- subsets (divisors n), sum x == n ] /= [] = True
    | otherwise = False

--Es 3.1
data  Either' a b  =  Left' a | Right' b deriving (Show)

--Definisco funtore
instance Functor (Either' a) where
    fmap f (Right' x) = Right' (f x)
    fmap f (Left' x) = Left' x

--e applivativo
instance Applicative (Either' e) where
    pure          = Right'
    Left'  e <*> _ = Left' e
    Right' f <*> r = fmap f r

--Istanza monade
instance Monad (Either' e) where
  Right' b >>= f = f b
  Left' a >>= _ = Left' a

data Term nat = Const nat | Div (Term nat)  (Term nat) | Plus (Term nat) (Term nat) | Sub (Term nat) (Term nat) | Time (Term nat) (Term nat)

data Exp = NotNaturalNumber | DivideByZero | BiggerSubtracting deriving (Show)

eval :: Integral b => Term b -> Either' Exp b
eval (Const a) = if a < 0 then Left' NotNaturalNumber else Right' a
eval (Div t u) = eval t >>= \a -> eval u >>= \b -> safeDiv a b
eval (Sub t u) = eval t >>= \a -> eval u >>= \b -> safeSub a b
eval (Plus t u) = eval t >>= \a -> eval u >>= \b -> Right' (a+b)
eval (Time t u) = eval t >>= \a -> eval u >>= \b -> Right' (a*b)


safeSub :: (Ord b, Num b) => b -> b -> Either' Exp b
safeSub a b
    |b > a = Left' BiggerSubtracting
    |otherwise = Right' (a-b)

safeDiv :: Integral b => b -> b -> Either' Exp b
safeDiv _ 0 = Left' DivideByZero
safeDiv a b = Right' (div a b)