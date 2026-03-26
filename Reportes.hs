module Reportes where

import Tipos
import FuncionesBasicas (mejorNota, peorNota)

-- Parte 5: REPORTES

-- Función para calcular promedio con Maybe
promedioMaybe :: [Double] -> Maybe Double
promedioMaybe [] = Nothing
promedioMaybe xs = Just (sum xs / fromIntegral (length xs))

-- Mostrar promedio desde lista
mostrarPromedio :: [Double] -> String
mostrarPromedio xs =
    case promedioMaybe xs of
        Nothing -> "Sin notas"
        Just p  -> show p

-- Mostrar Maybe
mostrarMaybe :: Maybe Double -> String
mostrarMaybe Nothing  = "Sin notas"
mostrarMaybe (Just x) = show x

-- Estado del estudiante
estadoEstudiante :: Estudiante -> String
estadoEstudiante est =
    case promedioMaybe (califs est) of
        Nothing -> "Sin notas"
        Just p  -> if p >= 3.0 then "Aprobado" else "Reprobado"

-- REPORTE DE ESTUDIANTE 
reporteEstudiante :: Estudiante -> String
reporteEstudiante est =
    "REPORTE DEL ESTUDIANTE\n" ++
    "Codigo: " ++ codigo est ++ "\n" ++
    "Nombre: " ++ nombreEst est ++ "\n" ++
    "Calificaciones: " ++ show (califs est) ++ "\n" ++
    "Cantidad de calificaciones: " ++ show (length (califs est)) ++ "\n" ++
    "Promedio: " ++ mostrarPromedio (califs est) ++ "\n" ++
    "Mejor nota: " ++ mostrarMaybe (mejorNota est) ++ "\n" ++
    "Peor nota: " ++ mostrarMaybe (peorNota est) ++ "\n" ++
    "Estado: " ++ estadoEstudiante est ++ "\n"

-- REPORTE DE MATERIA 
reporteMateria :: Materia -> String
reporteMateria mat =
    "REPORTE DE MATERIA\n" ++
    "Nombre: " ++ nombreMat mat ++ "\n" ++
    "Creditos: " ++ show (creditos mat) ++ "\n" ++
    "Cantidad de estudiantes: " ++ show (length (estudiantes mat)) ++ "\n\n" ++
    "ESTUDIANTES\n" ++
    if null (estudiantes mat)
       then "No hay estudiantes registrados\n"
       else concatMap (\e -> reporteEstudiante e ++ "\n") (estudiantes mat)