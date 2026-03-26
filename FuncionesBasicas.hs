mmodule FuncionesBasicas where

import Tipos

-- 
-- PARTE 2 - FUNCIONES BASICAS
-- 

-- Promedio simple
promedio :: [Double] -> Maybe Double
promedio [] = Nothing
promedio xs = Just (sum xs / fromIntegral (length xs))

-- Verifica si aprueba
estaAprobado :: Estudiante -> Bool
estaAprobado e =
  case promedio (califs e) of
    Just p  -> p >= 3.0
    Nothing -> False

-- Mejor nota
mejorNota :: Estudiante -> Maybe Double
mejorNota e
  | null (califs e) = Nothing
  | otherwise       = Just (maximum (califs e))

-- Peor nota
peorNota :: Estudiante -> Maybe Double
peorNota e
  | null (califs e) = Nothing
  | otherwise       = Just (minimum (califs e))

-- Cantidad de notas
cantidadCalifs :: Estudiante -> Int
cantidadCalifs e = length (califs e)


-- 
-- PARTE 10 - LIMITE DE NOTAS
--

-- Verifica si puede agregar otra nota (max 5)
puedeAgregarNota :: Estudiante -> Bool
puedeAgregarNota e = length (califs e) < 5


-- 
-- PARTE 11 - PONDERACION
-- 

type Calificacion = (Double, Double) -- (nota, peso)

-- Promedio ponderado
promedioPonderado :: [Calificacion] -> Maybe Double
promedioPonderado [] = Nothing
promedioPonderado xs =
  let sumaPesos = sum (map snd xs)
      sumaTotal = sum (map (\(n,p) -> n * p) xs)
  in if sumaPesos == 0
        then Nothing
        else Just (sumaTotal / sumaPesos)