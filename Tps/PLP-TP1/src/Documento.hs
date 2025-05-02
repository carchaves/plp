module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

-- Ejercicio 1
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea Vacio = cVacio -- Caso base Vacio
foldDoc cVacio cTexto cLinea (Texto s d) = cTexto s (foldDoc cVacio cTexto cLinea d) -- Caso recursivo Texto
foldDoc cVacio cTexto cLinea (Linea i d) = cLinea i (foldDoc cVacio cTexto cLinea d) -- Caso recursivo Linea

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

--  Ejercicio 2
(<+>) :: Doc -> Doc -> Doc
(<+>) d1 d2 =
  foldDoc
    d2 -- Caso Vacio: Devuelvo d2.
    ( \s1 rec ->
        case rec of -- Si el llamado recursivo es de tipo Texto ...
          Texto s2 d2' -> Texto (s1 ++ s2) d2' -- concateno el string del llamado recursivo (s2), al string de la función lambda (s1) ...
          _ -> Texto s1 rec -- caso Contrario, sigue la recursion.
    ) -- Caso Texto
    Linea -- Caso Linea: Agrega la linea y sigue la recursion.
    d1

-- Invariante
-- Dados dos documentos d1 y d2 que cumplen con el invariante porque se construyen a partir de funciones que
-- mantienen el invariante (vacio, texto, linea). Queremos ver que d1 <+> d2 tambien cumple con el invariante.

-- Nuestra implementacion de (<+>) usa foldDoc, sabemos que la funcion es aplicada a cada constructor de d1
-- de la siguiente manera: d1 <+> d2 = (d1^(1) <+> (d1^(2) <+> (... <+> (d1^(n) <+> d2)))), donde d1^(1), d1^(2) ... d1^(n)
-- son los subdocumentos que componen a d1.
-- Por lo tanto basta con mostrar que se cumple el invariante para cada constructor de un documento.

-- Si d1 es Vacio: devolvemos d2, que cumple con el invariante. Por lo tanto, cumple el invariante.

-- Si d1 es (Linea i d): agregamos la linea sin modificar el numero, por lo que si el numero era mayor o igual a 0,
-- lo seguira siendo despues de la concatenacion. Por lo tanto, cumple con el invariante.

-- Si d1 es (Texto s d): Tenemos dos casos:

-- 1) Si d es Texto: concatenamos los strings, como partimos de documentos que cumplen el invariante, sabemos que
-- ninguno de los strings son vacios ni contienen saltos de linea. Despues de concatenar los strings (++), agregamos
-- el al resto del documento. Como concatenamos los dos textos en uno solo, sabemos que se mantiene la propiedad
-- de que para todo Texto s d, d es Vacio o Linea i d'. por lo tanto la concatenacion de los mismos mantiene el
--  invariante.

-- 2) Caso contrario: concatenamos el texto (<+>) sin modificar el string s, por lo que sigue cumpliendo el invariante.

-- Ejercicio 3
indentar :: Int -> Doc -> Doc
indentar i =
  foldDoc
    Vacio -- Caso Vacio
    Texto -- Caso Texto
    (\i' rec -> Linea (i + i') rec) -- Caso Linea

-- Invariante
-- Dado un documento d que cumple con el invariante y un numero positivo i.
-- Queremos ver que indentar d i mantiene el invariante.

-- Nuestra implementacion de indentar usa foldDoc, sabemos que la funcion es aplicada a cada elemento de d
-- de la siguiente manera: indentar i d = indentar i d' (indentar i d'' (... (indentar i d^(n))))),
-- donde d', d'' ... d^(n) son los subdocumentos que componen a d.
-- Por lo tanto basta con mostrar que se cumple el invariante para cada caso particular de un documento.

-- Si d es (Texto s d') o Vacio: No modifica nada, mantiene el invariante.

-- Si d es (Linea i d'): Substituye i por i+i', como ambos son numeros no negativos, la suma sigue siendo no
-- negativa, por lo tanto, mantiene el invariante.

-- Ejercicio 4
mostrar :: Doc -> String
mostrar =
  foldDoc
    "" -- Caso Vacio
    (++) -- Caso Texto
    (\i rec -> "\n" ++ nEspacios [1 .. i] ++ rec) -- Caso Linea

nEspacios :: [Int] -> String
nEspacios = foldr (\x rec -> " " ++ rec) ""

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def j

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
