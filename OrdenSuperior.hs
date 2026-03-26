module OrdenSuperior where

import Tipos
import FuncionesBasicas


-- Estudiantes Aprobados

estudiantesAprobados :: Materia -> [Estudiante]
estudiantesAprobados materia =
    filter estaAprobado (estudiantes materia)


-- Estudiantes Reprobados

estudiantesReprobados :: Materia -> [Estudiante]
estudiantesReprobados materia = filter (\estudiante -> not (estaAprobado estudiante)) (estudiantes materia)


-- Promedio de la materia cursada

promedioMateria :: Materia -> Maybe Double
promedioMateria materia =
    let listaProm = map (\estudiante -> promedio (califs estudiante)) (estudiantes materia)
        soloProm = [p | Just p <- listaProm]
    in if length soloProm == 0
       then Nothing
       else Just (sum soloProm / fromIntegral (length soloProm))


-- Nombres de estudiantes aprobados

nombresAprobados :: Materia -> [String]
nombresAprobados materia = map (\estudiante -> nombreEst estudiante) (estudiantesAprobados materia)


-- Calificaciones válidas

calificacionesValidas :: Estudiante -> [Double]
calificacionesValidas estudiante = filter (\c -> c >= 0.0 && c <= 5.0) (califs estudiante)


-- Tabla de promedios 

tablaPromedios :: Materia -> [(String, Double)]
tablaPromedios materia =
    [ (nombreEst estudiante, p)
    | estudiante <- estudiantes materia
    , Just p <- [promedio (califs estudiante)]
    ]