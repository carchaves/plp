max2 :: (Float,Float) -> Float
max2 (x, y) 
    | x >= y = x
    | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract_delejercicio :: Float -> Float -> Float
subtract_delejercicio = flip (-)

predecesor :: Float -> Float
predecesor = subtract_delejercicio 1

evaluarEnCero ::(Float -> Bool) -> Bool 
evaluarEnCero = \f -> f 0

dosVeces :: (Float -> Float) -> Float -> Float
dosVeces = \f -> f . f

flipAll :: [(a->b->c)] -> [(b->a->c)] 
flipAll = map flip

flipRaro :: b->(a->b->c)->a->c
flipRaro = flip (flip )

