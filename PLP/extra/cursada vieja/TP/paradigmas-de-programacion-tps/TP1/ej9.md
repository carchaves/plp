## ∀t :: AT a . ∀x :: a . ( elem x (preorder t) = elem x (postorder t) )

Tenemos que t es un arbol ternario, puedo usar el principio de inducción sobre esta estructura:

```haskell
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
```

Lo planteamos como un predicado unario:

```haskell
P(t) = elem x (preorder t) = elem x (postorder t)
```

Sabiendo el tipo de dato, podemos ver que el caso base es el Nil, mientras que el Tern es el caso inductivo. Primero demostremos esta propiedad para el caso base.

### Caso base: Nil

Primero recordemos la implementación de las funciones presentes:

```haskell
foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT cTern cNil at = case at of
  Nil -> cNil
  (Tern r i c d) -> cTern r (recu i) (recu c) (recu d)
  where recu = foldAT cTern cNil

preorder :: Procesador (AT a) a
preorder = foldAT (\rr ri rc rd -> rr:(ri ++ rc ++ rd)) []

postorder :: Procesador (AT a) a
postorder = foldAT (\rr ri rc rd -> (ri ++ rc ++ rd) ++ [rr]) []

elem :: Eq a => a -> [a] -> Bool
elem e [] = False
elem e (x:xs) = (e == x) || elem e xs
```

En este caso t es Nil, lo que nos dejaría con:

```haskell
elem x (preorder Nil) = elem x (postorder Nil)
```

Ahora desarrollemos las funciones de preorder y postorder:

```haskell
elem x (foldAT (\rr ri rc rd -> rr:(ri ++ rc ++ rd)) [] Nil) = elem x (foldAT (\rr ri rc rd -> (ri ++ rc ++ rd) ++ [rr]) [] Nil)
```

Vemos que en ambos casos se utiliza un foldAT, el cual según su definición se reduce al segundo argumento que se le es enviado cuando su tercer argumento es Nil (basicamente se reduce a lista vacía).

```haskell
(elem x [] = elem x [])
```

Vemos que a ambos lados de la igualdad se tiene exactamente la misma expresión, por lo tanto se cumple la igualdad y la propiedad vale para el caso base.

## Caso inductivo: Tern

En este caso t es Tern a (AT a) (AT a) (AT a), haremos el siguiente cambio para este paso de la demostración, diremos que la propiedad vale para todo "i", "c" y "d" de tipo "AT a" y también para un e cuyo tipo sea a. Es decir:

```haskell
∀i :: AT a . ∀c :: AT a . ∀d :: AT a . ∀i :: AT a . ∀x :: a . ∀e :: a . (elem x (preorder (Tern e i c d)) = elem x (postorder (Tern e i c d)))
```

Primero tengamos en cuenta que por la Hipotesis inductiva la propiedad que queremos demostrar vale para i, c y d.

### HI: P(i) ∧ P(c) ∧ P(d)

Que sería lo mismo que decir que lo siguiente vale, siendo t en ese caso i, c o d.

```haskell
∀t :: AT a . ∀x :: a . ( elem x (preorder t) = elem x (postorder t) )
```

Ahora podemos ver que es lo que hacen la función preorder y postorder con el arbol.

```haskell
elem x (foldAT (\rr ri rc rd -> rr:(ri ++ rc ++ rd)) [] (Tern e i c d)) = elem x (foldAT (\rr ri rc rd -> (ri ++ rc ++ rd) ++ [rr]) [] (Tern e i c d))
```

En este caso vemos que por definción del foldAT, se le debe aplicar la función lambda descrita a los parametros.

Para simplificar los siguientes pasos solo vamos a desarrollar la parte izquierda de la igualdad.

```haskell
elem x (e:((recu i) ++ (recu c) ++ (recu d)))
    where recu = foldAT (\rr ri rc rd -> rr:(ri ++ rc ++ rd)) []
```

Pero ahora podriamos hacer una simplificación viendo que recu es exactamente la definición de preorder, por lo cual lo vuelvo a reemplazar.

```haskell
elem x (e:((preorder i) ++ (preorder c) ++ (preorder d)))
```

Antes de avanzar aplicando la definición de elem, veamos como desarrollar el lado derecho del igual.

```haskell
elem x (foldAT (\rr ri rc rd -> (ri ++ rc ++ rd) ++ [rr]) [] (Tern e i c d))
```

Aplicamos la definición de foldAT con lo que dice la función lambda:

```haskell
elem x ((recu i) ++ (recu c) ++ (recu d)) ++ [e]
    where recu = foldAT (\rr ri rc rd -> (ri ++ rc ++ rd) ++ [rr]) []
```

Similar al caso anterior, vemos que recu es la definción de postorder, por lo que podemos simplificarlo reemplazando nuevamente.

```haskell
elem x ((postorder i) ++ (postorder c) ++ (postorder d)) ++ [e]
```

Ahora volvemos a la igualdad:

```haskell
elem x (e:((preorder i) ++ (preorder c) ++ (preorder d))) = elem x ((postorder i) ++ (postorder c) ++ (postorder d)) ++ [e]
```



## Borrador de ideas de como continuar


Podríamos aplicar la definición de elem para comprar los primeros dos elementos, en el caso de la derecha, estaríamos comparando directamente x con e lo que nos hace usar extensionalidad sobre booleanos, es decir fijarnos en los casos donde la comparación es verdadera o falsa, si fuera verdadera, sabemos que si en una disyunción hay al menos un solo true, entonces la expresión se reducirá a true, por lo que podriamos reemplazarlo con eso.  
En el lado de la derecha tenemos que la e está al final de todo, una cosa que pensé es que aun suponiendo que todos los elementos de postorder seran distintos a x, el último tendrá que ser necesariamente igual porque estamos analizando el caso en el que es verdadera, devuelta lo reducimos a true por propiedad de la disyuncion y tenemos true de ambos lados.

El problema con esto es que si e no es igual.  

Otra cosa que pensé es que por la hipotesis inductiva, podriamos decir que los preorder y postorder tienen los mismos elementos reordenados de forma diferente

Por lo que quiza podriamos intentar demostrar un lema que diga algo así:

elem x xs || elem x ys = elem x (xs ++ ys)

Si logramos demostrar este lema, podriamos avanzar diciendo que por definición postorder y preorder terminan dando listas, por lo que aplicando el lema se puede reemplazar. Luego podriamos aplicar las hipotesis inductivas y reemplazar cada (elem x (postorder t)) por (elem x (preorder t)) siedo t : i, c y d.  

Otra opcion podria ser la de demostrar que:

elem x e:xs = elem x xs++[e]

así podemos pasar el e del final al principio.







