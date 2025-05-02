## Ejercicio 10

* El principio de reemplazo nos dice que si el programa declara que __e1 = e2__, cualquier instancia de e1 es igual a la correspondiente instancia de e2, y viceversa.

__Ecuaciones:__

```
indentar :: Int -> Doc -> Doc
{I0} indentar i = foldDoc Vacio Texto (\i' rec -> Linea (i + i') rec) 

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
{F0} foldDoc cVacio cTexto cLinea Vacio = cVacio 
{F1} foldDoc cVacio cTexto cLinea (Texto s d) = cTexto s (foldDoc cVacio cTexto cLinea d)
{F2} foldDoc cVacio cTexto cLinea (Linea i d) = cLinea i (foldDoc cVacio cTexto cLinea d) 

```
__Demostracion de lemas:__

1. _{L1}_
```
indentar k Vacio = Vacio para todo k :: Int positivo.

indentar k Vacio \
= foldDoc Vacio Texto (\i' rec -> Linea (k + i') rec) Vacio // Por {I0}
= Vacio                                                     // Por {F0}
```

2. _{L2}_
```
indentar k (Texto s d) = Texto s (indentar k d) para todo k :: Int positivo, s :: String y d :: Doc.

indentar k (Texto s d)
= foldDoc Vacio Texto (\i' rec -> Linea (k + i') rec) (Texto s d) // Por {I0}
= Texto s (foldDoc Vacio Texto (\i' rec -> Linea (k + i') rec) d) // Por {F1}
= Texto s (indentar k d)                                          // Por {I0}
∀k :: Int, s :: String, d :: Doc
```

3. _{L3}_
```
indentar m (Linea k d) = Linea (m+k) (indentar m d) para todo m, k :: Int positivos y d :: Doc.

indentar m (Linea k d)
= foldDoc Vacio Texto (\i' rec -> Linea (m + i') rec) (Linea k d)                           // Por {I0}
= (\i' rec -> Linea (m + i') rec) k (foldDoc Vacio Texto (\i' rec -> Linea (m + i') rec) d) // Por {F2}
= (\i' rec -> Linea (m + i') rec) k (indentar m d)                                          // Por {I0}
= (\rec -> Linea (m + k) rec) (indentar m d)                                                // Por β 
= Linea (m + k) (indentar m d)                                                              // Por β
```
\
 Se pide demostar utilizando razonamiento ecuacional e induccion estructural que para todo
Queremos probar que:
> ∀ n, m :: Int positivos,∀ x :: Doc. indentar n (indentar m x) = indentar (n+m) x

Definimos la siguiente propiedad:
P(x) ≡ ∀ n, m :: Int positivos. indentar n (indentar m x) = indentar (n+m) x

Por principio de induccion estructural, basta probar que:
1. P(Vacio)
2. ∀s :: String, ∀d :: Doc. P(d) -> P(Texto s d)
3. ∀i :: Int positivo, ∀d :: Doc. P(d) -> P(Linea i d)

Asumimos que (∀ n, m :: Int positivos) esta en todos los pasos.


1. 
```
P(Vacio) ≡ indentar n (indentar m Vacio) = indentar (n+m) Vacio

Izq.
indentar n (indentar m Vacio)
= indentar n Vacio  // Por {L1}
= Vacio             // Por {L1}

Der.
indentar (n+m) Vacio
= Vacio             // Por {L1}
```
_Q.E.D._


2. 
Queremos demostrar la Tesis, asumimos que se cumple la H.I. \
__Hipotesis Inductiva:__ P(d) ≡ indentar n (indentar m d) = indentar (n+m) d \
__Tesis Inductiva:__ P(Texto s d) ≡ indentar n (indentar m (Texto s d)) = indentar (n+m) (Texto s d) \

```
indentar n (indentar m (Texto s d))
= indentar n (Texto s (indentar m d)) // Por {L2}
= Texto s (indentar n (indentar m d)) // Por {L2}
= Texto s (indentar (n+m) d)          // Por {H.I.}
= indentar (n+m) (Texto s d)          // Por {L2}
∀ n, m :: Int positivos
```
Q.E.D


3. 
Queremos demostrar la Tesis, asumimos que se cumple la H.I.\
__Hipotesis Inductiva:__ P(d) ≡ indentar n (indentar m d) = indentar (n+m) d\
__Tesis Inductiva:__ P(Linea i d) ≡ indentar n (indentar m (Linea i d)) = indentar (n+m) (Linea i d)

```
indentar n (indentar m (Linea i d))
= indentar n (Linea (m+i) (indentar m d))     // Por {L3}
= Linea (n+(m+i)) (indentar n (indentar m d)) // Por {L3}
= Linea (n+(m+i)) (indentar (n+m) d)          // Por {H.I.}
= Linea ((n+m)+i) (indentar (n+m) d)          // Por asociatividad de la suma
= indentar (n+m) (Linea i d)                  // Por {L3}
∀ n, m :: Int positivos
```
Q.E.D

* Por lo tanto, queda probado que \
∀ n, m :: Int positivos,∀ x :: Doc. indentar n (indentar m x) = indentar (n+m) x