max2 :: (Float, Float) -> Float
max2 (x,y)
    | x >= y = x
    | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x,y) = sqrt (x^2 + y^2)

-- -- duda
subtract :: Float -> Float -> Float -- a -> b -> c
subtract = flip (-) 

-- predecesor :: Float -> Float -> Float -> Float 
-- predecesor = subtract 1 

evaluarEnCero :: (Float -> b) -> b
evaluarEnCero = \f -> f 0

dosVeces :: (Float -> Float) -> Float -> Float 
dosVeces = \f -> f . f

flipAll :: [a->b->c] -> [b-> a -> c]
flipAll  = map flip 

flipRaro :: (b -> a -> c) 
flipRaro f = flip flip f

-- (a -> b -> c) -> (b -> a -> c)
-- (d -> e -> f) -> (e -> d -> f)