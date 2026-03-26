module FuncionesBasicas where
import Tipos

-- Parte 2

promedio :: [Double] -> Maybe Double
promedio [] = Nothing
promedio xs = Just (sum xs / fromIntegral (length xs))

cantidadCalifs :: Estudiante -> Int
cantidadCalifs e = length (califs e)

estaAprobado :: Estudiante -> Bool
estaAprobado e =
  case promedio (califs e) of
    Just p -> p >= 3
    Nothing -> False

mejorNota :: Estudiante -> Maybe Double
mejorNota e =
  if null (califs e)
  then Nothing
  else Just (maximum (califs e))

peorNota :: Estudiante -> Maybe Double
peorNota e =
  if null (califs e)
  then Nothing
  else Just (minimum (califs e))

-- Parte 10 (maximo 5 notas)

puedeAgregarNota :: Estudiante -> Bool
puedeAgregarNota e = length (califs e) < 5

-- Parte 11 (ponderación)

type Calificacion = (Double, Double)

promedioPonderado :: [Calificacion] -> Maybe Double
promedioPonderado [] = Nothing
promedioPonderado xs =
  let suma = sum [n * p | (n,p) <- xs]
      pesos = sum [p | (_,p) <- xs]
  in Just (suma / pesos)