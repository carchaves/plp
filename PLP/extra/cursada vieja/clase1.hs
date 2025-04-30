--maximo = mejorSegun (>)

mejorSegun ::(a -> a -> Bool) -> [a] -> a 
mejorSegun _ [x] = x
mejorSegun m (x:xs) =
    if m x (mejorSegun m xs) then x else mejorSegun m xs

--shuffle
shuffle :: [Int] -> [a] -> [a]
shuffle [] _ = []
shuffle (x:xs) ys = ys !! x : shuffle xs ys

shuffle :: [Int] -> [a] -> [a]
shuffle indices lista = map (lista !!) indices 

 