module Validaciones where

import Tipos


--
-- PARTE 4 - VALIDACION
--

-- Validar calificación

validarCalificacion :: Double -> Either String Double
validarCalificacion nota
    | nota < 0.0 = Left "La calificación es menor que 0.0"
    | nota > 5.0 = Left "La calificación es mayor que 5.0"
    | otherwise  = Right nota



-- Validar estudiante

validarEstudiante :: Estudiante -> Either String Estudiante
validarEstudiante estudiante =
    let notasInvalidas = filter (\nota -> nota < 0.0 || nota > 5.0) (califs estudiante)
    in if null notasInvalidas
       then Right estudiante
       else Left "El estudiante tiene notas fuera del rango entre 0.0 y 5.0"


-- Agregar calificación

agregarCalificacion :: Double -> Estudiante -> Either String Estudiante
agregarCalificacion nota estudiante =
    case validarCalificacion nota of
        Left err -> Left ("No se pudo agregar la nota: " ++ err)
        Right notaCorrecta ->
            Right estudiante { califs = califs estudiante ++ [notaCorrecta] }



-- 
-- PARTE 8 - VALIDACIONES AVANZADAS
-- 

-- Validar un estudiante con mensajes personalizados

validarEstudianteAvanzado :: Estudiante -> Either String Estudiante
validarEstudianteAvanzado estudiante
    | null (nombreEst estudiante) = Left "El estudiante no tiene nombre"
    | null (califs estudiante)    = Left "El estudiante no tiene calificaciones"
    | otherwise =
        case validarEstudiante estudiante of
            Left err -> Left ("Hubo un error en las calificaciones: " ++ err)
            Right est -> Right est


-- Validar una materia completa con mensajes personalizados

validarMateriaAvanzada :: Materia -> Either String Materia
validarMateriaAvanzada materia =
    case validarCodigosUnicos materia of
        Left err -> Left ("Hubo un error en los códigos: " ++ err)
        Right mat ->
            let resultados = map validarEstudianteAvanzado (estudiantes mat)
                errores = [e | Left e <- resultados]
            in if null errores
               then Right mat
               else Left ("Hubo errores en estudiantes:\n" ++ unlines errores)


-- Agregar una calificación con mensaje personalizado

agregarCalificacionAvanzado :: Double -> Estudiante -> Either String Estudiante
agregarCalificacionAvanzado nota estudiante =
    case agregarCalificacion nota estudiante of
        Left err -> Left ("No se pudo agregar la calificación: " ++ err)
        Right est -> Right est




--
-- PARTE 12 - DUPLICADOS
--

-- Validar que no haya códigos duplicados en una materia

validarCodigosUnicos :: Materia -> Either String Materia
validarCodigosUnicos materia =
    let codigos = map codigo (estudiantes materia)
    in if hayDuplicados codigos
       then Left "Hay estudiantes que tienen códigos repetidos"
       else Right materia



-- Función recursiva para detectar duplicados

hayDuplicados :: Eq a => [a] -> Bool
hayDuplicados [] = False
hayDuplicados (x:xs)
    | x `elem` xs = True
    | otherwise   = hayDuplicados xs