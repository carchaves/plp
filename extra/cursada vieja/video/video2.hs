type Conj a = a -> Bool

c :: Conj Int
c = (\x -> x `mod` 2 == 1)

vacio:: Conj a
vacio = (\x -> False)

agregar :: Eq a => a -> Conj a -> Conj a
agregar x c = x == a | Conj a


