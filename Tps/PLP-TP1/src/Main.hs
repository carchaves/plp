module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 1" ~: testsEj1,
      "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 5" ~: testsEj5,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

doc1 = texto "Que" <+> linea <+> texto "buen " <+> linea <+> texto "equipo"

testsEj1 :: Test
testsEj1 =
  test
    [ foldDoc vacio (\s rec -> texto s <+> rec) (\i rec -> indentar i linea <+> rec) doc1 ~=? doc1,
      foldDoc vacio (\s rec -> texto "MILF" <+> rec) (\i rec -> indentar i linea <+> rec) doc1 ~=? texto "MILF" <+> linea <+> texto "MILF" <+> linea <+> texto "MILF",
      foldDoc vacio (\s rec -> vacio) (\i rec -> vacio) doc1 ~=? vacio,
      foldDoc "" (++) (\i rec -> show i ++ rec) (indentar 3 doc1) ~=? "Que3buen 3equipo"
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),
      -- Tests propios
      texto "a" <+> vacio ~?= texto "a",
      vacio <+> texto "a" ~?= texto "a"
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      -- Tests propios
      indentar 2 (linea <+> linea) ~?= indentar 1 (indentar 1 linea <+> indentar 1 linea)
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      -- Tests propios
      mostrar (indentar 2 (texto "a" <+> linea)) ~?= "a\n  ",
      mostrar (indentar 2 (linea <+> linea)) ~?= "\n  \n  ",
      mostrar (indentar 2 (linea <+> texto "a")) ~?= "\n  a"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

grupo, nota, mejorGrupo, podio :: PPON
grupo = TextoPP "MILF"
nota = IntPP 10
mejorGrupo = ObjetoPP [("Grupo", grupo), ("Nota", nota)]
podio = ObjetoPP [("1", mejorGrupo)]

testsEj5 :: Test
testsEj5 =
  test
    [ pponAtomico grupo ~=? True,
      pponAtomico grupo ~=? True,
      pponAtomico mejorGrupo ~=? False
    ]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      -- Tests propios
      pponObjetoSimple grupo ~=? False,
      pponObjetoSimple grupo ~=? False,
      pponObjetoSimple mejorGrupo ~=? True
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      -- Tests propios
      mostrar (intercalar (texto " ") [a, b, c]) ~?= "a b c",
      mostrar (entreLlaves [a, indentar 1 linea, indentar 1 linea]) ~?= "{\n  a,\n  \n   ,\n  \n   \n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      -- Tests propios
      mostrar (aplanar (a <+> linea <+> linea <+> b)) ~?= "a  b",
      mostrar (aplanar (a <+> linea <+> linea <+> b <+> linea)) ~?= "a  b ",
      mostrar (aplanar (entreLlaves [a, indentar 1 linea, indentar 1 linea])) ~=? "{ a,  ,   }",
      mostrar (aplanar (linea <+> c)) ~?= " c",
      mostrar (aplanar (linea <+> linea <+> c)) ~?= "  c"
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      -- Tests propios
      mostrar (pponADoc merlina) ~?= "{ \"nombre\": \"Merlina\", \"edad\": 24 }",
      mostrar (pponADoc mejorGrupo) ~?= "{ \"Grupo\": \"MILF\", \"Nota\": 10 }",
      mostrar (pponADoc podio) ~=? "{\n  \"1\": { \"Grupo\": \"MILF\", \"Nota\": 10 }\n}"
    ]
