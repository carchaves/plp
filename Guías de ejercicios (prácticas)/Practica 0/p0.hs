-- 1
-- null:: [a] -> Bool
-- head:: [a] -> a
-- tail:: [a] -> [as]
-- init:: [a] -> [as]
-- last:: [a] -> a
-- take:: a -> [a] -> [a]
-- drop:: a -> [a] -> [a]
-- (++):: [a] -> [a] -> [a]
-- concat::[[a]] -> [a]
-- reverse:: [a] -> [a]
-- elem:: a -> [a] -> Bool

-- 2
-- a
valorAbsoluto :: Float -> Float
valorAbsoluto a =
    if a<0 then -a else a

-- b
bisiesto :: Int -> Bool
bisiesto a = mod a 4 == 0

-- c
factorial :: Int -> Int
factorial 0 = 1
factorial a = a* factorial (a-1)

-- d

primo1 :: Int -> Int -> Bool
primo1 a 1 = True
primo1 a b = (mod a b /= 0) && primo1 a (b-1)

primo :: Int -> Bool
primo 1 = False
primo a = primo1 a (a-1)

siguientePrimo :: Int -> Int
siguientePrimo a =
    if primo (a+1) then a+1 else siguientePrimo (a+1)

contarDivPrimos:: Int -> Int -> Int
contarDivPrimos a b
    | a==b && primo b = 1
    | a<=b = 0
    | mod a b == 0 = 1 + contarDivPrimos a (siguientePrimo b)
    | otherwise = contarDivPrimos a (siguientePrimo b)


cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos a = contarDivPrimos a 2

-- 3
-- a
inverso :: Float -> Maybe Float
inverso a
    | a == 0 = Nothing
    | otherwise = Just (1 / a)

-- b
aEntero :: Either Int Bool -> Int
aEntero (Right True) = 1
aEntero (Right False) = 0
aEntero (Left a) = a

-- 4
-- a
limpiar :: String -> String -> String
limpiar [] _ = []
limpiar a b
    | take (length b) a == b = limpiar (drop (length b) a) b
    | otherwise =  head a:limpiar (tail a) b

-- b

prom ::[Float] -> Float
prom [] = 0
prom a = sum a / fromIntegral (length a)

restaALista :: [Float] -> Float -> [Float]
restaALista [] _ = []
restaALista (a:as) b = a-b : restaALista as b

difPromedio :: [Float] -> [Float]
difPromedio a = restaALista a (prom a)

-- c

todosIguales :: [Int] -> Bool 
todosIguales [x] = True
todosIguales (x1:x2:xs)
    | x1==x2 = todosIguales(x2:xs)
    | otherwise = False

-- 5
data AB a = Nil | Bin (AB a) a (AB a)

instance Show a => Show (AB a) where
    show Nil = "Nil"
    show (Bin l n r) = "Bin (" ++ show l ++ ") " ++ show n ++ " (" ++ show r ++ ")"

-- a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

-- b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin l n r) = Bin (negacionAB l) (not n) (negacionAB r)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin l n r) = n * productoAB l* productoAB r


-- pseudo tarea hacer un merge

merge :: Ord a => [a] -> [a]-> [a]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

hallarMitad :: Ord a => [a] -> Int
hallarMitad a = div (length a) 2

mergesort :: Ord a => [a] -> [a]
mergesort [a] = [a]
mergesort a = merge (mergesort (take (hallarMitad a) a)) (mergesort (drop (hallarMitad a) a))

