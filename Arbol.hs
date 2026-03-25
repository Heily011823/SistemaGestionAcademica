module Arbol where 
import Tipos 

-- Parte 6: Definiciones de Árbol 
type ParPromedio = (Double, String) 

data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a) 
    deriving (Show, Eq)

-- Función para insertar en el árbol
insertarEnArbol :: ParPromedio -> Arbol ParPromedio -> Arbol ParPromedio
insertarEnArbol nuevo Vacio = Nodo nuevo Vacio Vacio
insertarEnArbol (p1, n1) (Nodo (p2, n2) izq der)
    | p1 >= p2  = Nodo (p2, n2) izq (insertarEnArbol (p1, n1) der)
    | otherwise = Nodo (p2, n2) (insertarEnArbol (p1, n1) izq) der

-- Función que genera el árbol desde la Materia
arbolDesdeMateria :: Materia -> Arbol ParPromedio
arbolDesdeMateria (Materia _ _ ests) = foldr insertar (Vacio) pares
  where
    pares = map (\e -> (sum (califs e) / fromIntegral (length (califs e)), nombreEst e)) ests
    insertar p arb = insertarEnArbol p arb

-- Función para obtener la lista final del ranking 
rankingEstudiantes :: Materia -> [(Double, String)]
rankingEstudiantes m = treeToList (arbolDesdeMateria m)
  where
    treeToList Vacio = []
    treeToList (Nodo x izq der) = treeToList der ++ [x] ++ treeToList izq