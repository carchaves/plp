-- 1
-- Ejercicio 1 ⋆
-- Considerar las siguientes definiciones de funciones:
-- - max2 (x, y) | x >= y = x
-- | otherwise = y
-- - normaVectorial (x, y) = sqrt (x^2 + y^2)
-- - subtract = flip (-)
-- - predecesor = subtract 1
-- - evaluarEnCero = \f -> f 0
-- - dosVeces = \f -> f . f
-- - flipAll = map flip
-- - flipRaro = flip flip


-- i. ¿Cuál es el tip o de cada función? (Sup oner que to dos los números son de tip o Float).

-- max2 :: Ord a => (a,a) -> a

-- normaVectorial :: (Float,Float) -> Float

-- subtract :: a -> a -> a

-- predecesor :: a -> a

-- evaluarEnCero :: (Int -> a) -> a

-- dosVeces :: (a -> a) -> a

-- flipAll :: [(a -> b -> c)] -> [(b -> a -> c)]

--flipRaro :: b -> (a -> b -> c) -> a -> c


-- ii. Indicar cuáles de las funciones anteriores no están currificadas. Para cada una de ellas, definir la función
-- currificada corresp ondiente. Recordar dar el tip o de la función.

--max2 y normaVectorial


--2

-- i. Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada

curry2 :: (a -> b -> c) -> (a,b) -> c
curry2 f (a,b) = f a b

-- ii. Definir la función uncurry, que dada una función currificada de dos argumentos, devuelve su versión no
-- currificada equivalente. Es la inversa de la anterior.

uncurry2 :: ((a,b) -> c) -> a -> b -> c
uncurry2 f a b = f (a,b)

--  iii. ¿Se p o dría definir una función curryN, que tome una función de un número arbitrario de argumentos y
-- devuelva su versión currificada?
-- Sugerencia: p ensar cuál sería el tip o de la función.

-- A menos de que halla una forma de crear tuplas de una cantidad finita de elementos no se puede definir


--3

--i. Redefinir usando foldr las funciones sum, elem, (++), filter y map
sum2 :: Num a => [a] -> a
sum2 = foldr (+) 0

--ii. Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
-- de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún (>).
mejorSegun :: (Ord a) => (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if (f x y) then x else y)

--iii. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
-- otra de la misma longitud, que tiene en cada p osición la suma parcial de los elementos de la lista original
-- desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ;[1,5,4,4,9]

sumasParciales :: Num a => [a] -> [a]
sumasParciales as = tail(foldl (\x y ->  x ++ [last x + y]) [0] as)

sumasParciales2 :: Num a => [a] -> [a]
sumasParciales2 = foldr (\ x rec -> x : (map (+x) rec)) []




--iv. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
--resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

sumaAlt2 :: (Num a) => [a] -> a
sumaAlt2 [] = 0
sumaAlt2 (x:x2:xs) = x - (sumaAlt xs)

sumaAlt3 :: (Num a) => [a] -> a
sumaAlt3 = foldr (\x rec -> x-rec) 0 


-- v. Hacer lo mismo que en el punto anterior, p ero en sentido inverso (el último elemento menos el anteúltimo,
-- etc.). Pensar qué esquema de recursión conviene usar en este caso.
sumaAltR :: Num a => [a] -> a
sumaAltR = foldl (flip (-)) 0



--4

--i. Defnir la función permutaciones :: [a] -> [[a]], que dada una lista devuelve todas sus permutacio-
--nes. Se recomienda utilizar concatMap ::(a->[b])-> [a] -> [b], y tambien take y drop.

permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rec -> concatMap (\i -> insertarEnTodasPos (x:i) ) rec) [[]]

insertarEnTodasPos :: [a] -> [[a]]
insertarEnTodasPos (x:xs) = [take i xs ++ [x] ++ drop i xs | i <-[0..length xs]] 




-- ii. Defnir la función partes, que recibe una lista L y devuelve la lista de todas las listas formadas por los
-- mismos elementos de L, en su mismo orden de aparición.
-- Ejemplo: partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
-- (en algún orden)
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = (map (x:) (partes xs))++(partes xs)

partes2 :: [a] -> [[a]]
partes2 = foldr (\x rec -> (map (x:) rec) ++ rec) [[]]

-- iii. Definir la función prefijos, que dada una lista, devuelve to dos sus prefijos.
-- Ejemplo: prefijos [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]

prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos (x:xs) = [[x]]++ (map (x:) (prefijos xs))

prefijos2 :: [a] -> [[a]]
prefijos2 = foldr ((\x rec -> [head rec] ++ (map (x:) rec))) ([[]])


--5.
-- Considerar las siguientes funciones:

--  elementosEnPosicionesPares :: [a] -> [a]
--  elementosEnPosicionesPares [] = []
--  elementosEnPosicionesPares (x:xs) = if null xs
    --  then [x]
    --  else x : elementosEnPosicionesPares (tail xs)

--  entrelazar :: [a] -> [a] -> [a]
--  entrelazar [] = id
--  entrelazar (x:xs) = \ys -> if null ys
    --  then x : entrelazar xs []
    --  else x : head ys : entrelazar xs (tail ys)

-- Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
-- En caso contrario, explicar el motivo.

-- elementosEnPosicionesPares utiliza recursión globla porque utliza recursion sobre la cola de la lista
-- entrelazar utiliza recursion global porque utliza recursion sobre la cola de la lista

--6.
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)


-- a. Definir la función sacarUna :: Eq a => a -> [a] -> [a], que dados un elemento y una lista devuelve el
-- resultado de eliminar de la lista la primera aparición del elemento (si está presente).
 
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs rec-> if n==x then xs else x:rec) []

sacarUna2 :: Eq a => a -> [a] -> [a]
sacarUna2 n [] = []
sacarUna2 n (x:xs) = if n == x then xs else x: (sacarUna2 n xs)

-- b. Explicar p or qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función
-- sacarUna del punto anterior.

-- el esquema foldr no es el adecuado pues en cada paso de recursion es necesario poseer la información de la cola de la lista

--c. Definr la función insertarOrdenado :: Ord a => a -> [a] -> [a] que inserta un elemento en una lista
-- ordenada (de manera creciente), de manera que se preserva el ordenamiento.

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr (\x xs rec ->(if n<x then n:x:xs else (if rec==[] then x:n:rec else x:rec))) [] 

-- Ejercicio 7 ⋆
-- Definir las siguientes funciones para traba jar sobre listas, y dar su tipo. Todas ellas deben poder aplicarse a
-- listas finitas e infinitas.

-- i. mapPares, una versión de map que toma una función currificada de dos argumentos y una lista de pares
-- de valores, y devuelve la lista de aplicaciones de la función a cada par. Pista: recordar curry y uncurry.

mapPares :: ((a,b) -> c) -> [(a,b)] -> [c]
mapPares f [] = [] 
mapPares f (x:xs) = (f x) : (mapPares f xs) 

-- ii. armarPares, que dadas dos listas arma una lista de pares que contiene, en cada posición, el elemento
-- correspondiente a esa posición en cada una de las listas. Si una de las listas es más larga que la otra,
-- ignorar los elementos que sobran (el resultado tendrá la longitud de la lista más corta). Esta función en
-- Haskell se llama zip. Pista: aprovechar la currificación y utilizar evaluación parcial.

armarPares :: [a] -> [a] -> [a]
armarPares [] [] = []
armarPares (x:xs) [] = x:(armarPares xs [])
armarPares [] (y:ys) = y:(armarPares [] ys)
armarPares (x:xs) (y:ys) = x:y:(armarPares xs ys)

--  iii. mapDoble, una variante de mapPares, que toma una función currificada de dos argumentos y dos listas
-- (de igual longitud), y devuelve una lista de aplicaciones de la función a cada elemento corresp ondiente de
-- las dos listas. Esta función en Haskell se llama zipWith.

mapDoble :: ((a,b) -> c) -> [a] -> [b] -> [c]
mapDoble f (a:as) (b:bs) = (curry f a b) : mapDoble f as bs

-- Ejercicio 8

-- i. Escribir la función sumaMat, que representa la suma de matrices, usando zipWith. Representaremos una
-- matriz como la lista de sus filas. Esto quiere decir que cada matriz será una lista finita de listas finitas,
-- to das de la misma longitud, con elementos enteros. Recordamos que la suma de matrices se define como
-- la suma celda a celda. Asumir que las dos matrices a sumar están bien formadas y tienen las mismas
-- dimensiones.

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat a b = zipWith (zipWith (+)) a b

--  ii. Escribir la función trasponer, que, dada una matriz como las del ítem i, devuelva su traspuesta. Es decir,
-- en la posición i, j del resultado está el contenido de la p osición j, i de la matriz original. Notar que si la
-- entrada es una lista de N listas, to das de longitud M , la salida deb e tener M listas, to das de longitud N .

trasponer :: [[Int]] -> [[Int]] 
trasponer x = foldr (\l rec -> zipWith (:) l rec) (replicate (length (head x)) []) x 

-- [1 2 3]    [1 4 7]
-- [4 5 6] -> [2 5 8]
-- [7 8 9]    [3 6 9]

-- Ejercicio 9 ⋆
-- i. Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
-- Haskell (la función va a estar definida sólo para los enteros mayores o iguales que 0).

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat _ v 0 = v
foldNat f v n = f n (foldNat f v (n-1))

-- II. Utilizando foldNat, definir la función potencia.

potencia :: Integer -> Integer -> Integer
potencia n m = foldNat (\m rec -> n * rec) 1 m

-- Ejercicio 10
-- I. Definir la función genLista :: a -> (a -> a) -> Integer -> [a], que genera una lista de una cantidad dada de elementos,
-- a partir de un elemento inicial y de una función de incremento entre los elementos de la lista. Dicha función de incremento,
-- dado un elemento de la lista, devuelve el elemento siguiente.

genLista :: a -> (a -> a) -> Integer -> [a]
genLista i f l = foldNat (\x rec -> i : (map f rec) ) [] l 

-- II. Usando genLista, definir la función desdeHasta, que dado un par de números (el primero menor que el segundo),
-- devuelve una lista de números consecutivos desde el primero hasta el segundo.

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta a b = genLista a (+1) (b-a+1)