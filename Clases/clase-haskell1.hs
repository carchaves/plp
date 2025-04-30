maximoL :: Ord a => [a] -> a
maximoL = foldr1 max

take1 :: Int -> [a] -> [a]
take1 n [] = []
take1 n (x:xs) = if n == 0 then [] else x:take1 (n-1) xs

take2 :: [a] -> Int -> [a]
take2 [] n = []
take2 (x:xs) n = if n == 0 then [] else x:take2 xs (n-1)

take3 :: [a] -> Int -> [a]
take3 [] = const []
take3 (x:xs) = (\n -> if n == 0 then [] else x:take3 xs (n-1))

takef :: [a] -> Int -> [a]
takef = foldr (\x rec -> (\n -> if n == 0 then [] else x:rec (n-1))) (const [])

-- sublistaQueMasSuma :: [Int] -> [Int]
-- sublistaQueMasSuma = foldr (\x res -> ) []

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

pares :: [(Int, Int)]
pares = [p | k <- [0..], p <- paresQueSuman k]

paresQueSuman :: Int -> [(Int, Int)]
paresQueSuman k = [(i, k-i) | i <- [0..k]]

data AEB a = Hoja a | Bin (AEB a) a (AEB a)
    deriving(Show)

foldAEB :: (a-> b) -> (b-> a -> b -> b) -> AEB a -> b
foldAEB fHoja fBin (Hoja a) = fHoja a
foldAEB fHoja fBin (Bin i r d) = 
    fBin (foldAEB fHoja fBin i) r (foldAEB fHoja fBin d)



-- cantHojas = foldAEB (const 1) (\resi _ resd -> resi + resd)

-- espejo :: AEB a -> AEB a
-- espejo = 
--     foldAEB
--         (\a -> Hoja a)
--         (\resi r resd ->
--             Bin resd r resi)


-- ramas :: AEB a -> [[a]]
-- ramas 



-- data AB a = Nil | Bin (AB a) a (AB a)
--     deriving (Show)

-- InsertarABB :: (Ord a) => AB a -> AB a
-- InsertarABB x Nil = Bin Nil x Nil
-- InsertarABB x (Bin i r d) =
--     if x < r
--         then Bin (InsertarABB x i) r d
--         else Bin i r (InsertarABB x d)

-- InsertarABB2 :: (Ord a) => AB a -> AB a
-- InsertarABB2 = foldAB





----------


-- data RoseTree a = Rose a [RoseTree a]

-- foldRT :: (a -> [b] -> b) -> RoseTree a -> b
-- foldRT fRose (Rose a hijos) = fRose a (map (foldRT fRose) hijos)

-- altura :: RoseTree a -> Int
-- altura = foldRT (\r alturaHijos -> 
--                     case alturaHijos of
--                         [] -> 
--                         _ _> maximum (map (+ 1) alturaHijos)  )


----
type Conj a = (a -> Bool)

pertenece :: a -> Conj a -> Bool
pertenece x f = f x

vacio :: Conj a
vacio _ = False

agregar :: (Eq a) => a -> Conj a -> Conj a
-- agregar x f = (\y -> if x == y then True else f y)
-- agregar x f = (\y -> x == y || f y)
agregar x f y = x == y || f y

intersec :: Conj a -> Conj a -> Conj agregar
intersec a b = a x %% b x