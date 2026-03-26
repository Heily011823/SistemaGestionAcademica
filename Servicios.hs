module Servicios where

import Tipos
import Validaciones

-- Actualiza materia aplicando una funcion
actualizarMateria :: Materia -> (Estudiante -> Either String Estudiante) -> Either [String] Materia
actualizarMateria mat f =
    let resultado = map f (estudiantes mat)
        errores = [e | Left e <- resultado]
    in if not (null errores)
        then Left errores
        else Right mat { estudiantes = [e | Right e <- resultado] }


-- Procesar agregar nota
procesarNota :: String -> Double -> Estudiante -> Either String Estudiante
procesarNota cod nota est
    | codigo est /= cod = Right est
    | otherwise =
        case agregarCalificacion nota est of
            Left err -> Left err
            Right nuevo ->
                Right nuevo {
                    historial = historial est ++ [AgregarNota nota]
                }


-- Procesar eliminar nota
procesarEliminar :: String -> Double -> Estudiante -> Either String Estudiante
procesarEliminar cod nota est
    | codigo est /= cod = Right est
    | otherwise = eliminarCalificacion nota est


-- Procesar modificar nota
procesarModificar :: String -> Double -> Double -> Estudiante -> Either String Estudiante
procesarModificar cod vieja nueva est
    | codigo est /= cod = Right est
    | otherwise = modificarCalificacion vieja nueva est