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
agregarCalificacion nota estudiante
    | length (califs estudiante) >= 5 =
        Left "No se pueden agregar más de 5 calificaciones"
    | otherwise =
        case validarCalificacion nota of
            Left err -> Left ("No se pudo agregar la nota: " ++ err)
            Right notaCorrecta ->
                Right estudiante { califs = califs estudiante ++ [notaCorrecta] }

-- Eliminar Calificación
eliminarCalificacion :: Double -> Estudiante -> Either String Estudiante
eliminarCalificacion nota est
    | not (nota `elem` califs est) =
        Left "La nota no existe en el estudiante"
    | otherwise =
        Right est {
            califs = eliminarUna nota (califs est),
            historial = historial est ++ [EliminarNota nota]
        }

-- elimina solo la primera ocurrencia (recursivo)
eliminarUna :: Double -> [Double] -> [Double]
eliminarUna _ [] = []
eliminarUna n (x:xs)
    | n == x    = xs
    | otherwise = x : eliminarUna n xs

-- Modificar Calificación
modificarCalificacion :: Double -> Double -> Estudiante -> Either String Estudiante
modificarCalificacion vieja nueva est
    | not (vieja `elem` califs est) =
        Left "La nota a modificar no existe"
    | otherwise =
        case validarCalificacion nueva of
            Left err -> Left err
            Right nva ->
                Right est {
                    califs = map (\x -> if x == vieja then nva else x) (califs est),
                    historial = historial est ++ [ModificarNota vieja nva]
                }

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

-- Función validar codigo estudiante

validarCodigo :: String -> Either String String
validarCodigo cod
    | length cod /= 6 = Left "El codigo debe tener exactamente 6 digitos"
    | not (all esDigito cod) = Left "El codigo solo debe contener numeros"
    | otherwise = Right cod

-- Funcion auxiliar recursiva para saber si es digito
esDigito :: Char -> Bool
esDigito c = c >= '0' && c <= '9'

