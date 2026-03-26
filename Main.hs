module Main where

import Tipos         
import FuncionesBasicas 
import Validaciones  
import Reportes      
import Arbol        
import OrdenSuperior  
import Servicios     

main :: IO ()
main = do
    putStrLn "SISTEMA DE GESTION ACADEMICA"
    let materiaInicial = Materia "Paradigmas" 4 []
    menuPrincipal materiaInicial


menuPrincipal :: Materia -> IO ()
menuPrincipal mat = do
    putStrLn "\nMENU PRINCIPAL"
    putStrLn "1. Agregar estudiante"
    putStrLn "2. Agregar calificacion"
    putStrLn "3. Eliminar calificacion"
    putStrLn "4. Modificar calificacion"
    putStrLn "5. Ver reportes"
    putStrLn "6. Ver ranking"
    putStrLn "7. Funciones avanzadas"
    putStrLn "8. Salir"
    opcion <- getLine

    case opcion of

        "1" -> agregarEstudiante mat
        "2" -> agregarNota mat
        "3" -> eliminarNota mat
        "4" -> modificarNota mat
        "5" -> verReportes mat
        "6" -> verRanking mat
        "7" -> submenuAvanzado mat
        "8" -> putStrLn "Saliendo..."
        _   -> menuPrincipal mat


--  FUNCIONES DE MENU

agregarEstudiante mat = do
    codInput <- pedir "Codigo:"

    case validarCodigo codInput of
        Left err -> print err >> menuPrincipal mat

        Right cod -> do
            nombre <- pedir "Nombre:"

            let existe = any (\e -> codigo e == cod) (estudiantes mat)

            if existe then
                putStrLn "Codigo duplicado" >> menuPrincipal mat
            else do
                let nuevo = Estudiante cod nombre [] []
                menuPrincipal mat { estudiantes = nuevo : estudiantes mat }


agregarNota mat = do
    cod <- pedir "Codigo:"
    nota <- fmap read (pedir "Nota:")
    manejarResultado mat (actualizarMateria mat (procesarNota cod nota))


eliminarNota mat = do
    cod <- pedir "Codigo:"
    nota <- fmap read (pedir "Nota a eliminar:")
    manejarResultado mat (actualizarMateria mat (procesarEliminar cod nota))


modificarNota mat = do
    cod <- pedir "Codigo:"
    vieja <- fmap read (pedir "Nota actual:")
    nueva <- fmap read (pedir "Nueva nota:")
    manejarResultado mat (actualizarMateria mat (procesarModificar cod vieja nueva))


verReportes mat = do
    putStrLn (reporteMateria mat)
    putStrLn "\nPromedio materia:"
    print (promedioMateria mat)
    menuPrincipal mat


verRanking mat = do
    putStrLn "\nRANKING:"
    let r = rankingEstudiantes mat
    if null r
        then putStrLn "Sin datos"
        else mapM_ (\(p,n) -> putStrLn (n ++ " -> " ++ show p)) r
    menuPrincipal mat


-- SUBMENU 
submenuAvanzado mat = do
    putStrLn "\nFUNCIONES AVANZADAS"
    putStrLn "1. Aprobados"
    putStrLn "2. Reprobados"
    putStrLn "3. Nombres aprobados"
    putStrLn "4. Tabla promedios"
    putStrLn "5. Calificaciones validas"
    putStrLn "6. Volver"

    op <- getLine

    case op of
        "1" -> print (estudiantesAprobados mat) >> submenuAvanzado mat
        "2" -> print (estudiantesReprobados mat) >> submenuAvanzado mat
        "3" -> print (nombresAprobados mat) >> submenuAvanzado mat
        "4" -> print (tablaPromedios mat) >> submenuAvanzado mat
        "5" -> do
            mapM_ (\e -> print (nombreEst e, calificacionesValidas e)) (estudiantes mat)
            submenuAvanzado mat
        "6" -> menuPrincipal mat
        _   -> submenuAvanzado mat


-- HELPERS

pedir msg = putStrLn msg >> getLine

manejarResultado mat resultado =
    case resultado of
        Left errs -> mapM_ putStrLn errs >> menuPrincipal mat
        Right nueva -> putStrLn "Operacion exitosa" >> menuPrincipal nueva