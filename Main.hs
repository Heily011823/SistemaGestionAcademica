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
        _   -> putStrLn "Opcion invalida" >> menuPrincipal mat


-- AGREGAR ESTUDIANTE
agregarEstudiante :: Materia -> IO ()
agregarEstudiante mat = do
    codInput <- pedir "Codigo:"

    case validarCodigo codInput of
        Left err -> do
            putStrLn err
            menuPrincipal mat

        Right cod -> do
            nombre <- pedir "Nombre:"

            let nuevo = Estudiante cod nombre [] []
            let nuevaMateria = mat { estudiantes = nuevo : estudiantes mat }

            case validarCodigosUnicos nuevaMateria of
                Left err -> do
                    putStrLn err
                    menuPrincipal mat

                Right matValida -> do
                    putStrLn "Estudiante agregado"
                    menuPrincipal matValida


-- AGREGAR NOTA
agregarNota :: Materia -> IO ()
agregarNota mat = do
    cod <- pedir "Codigo:"

    let existe = any (\e -> codigo e == cod) (estudiantes mat)

    if not existe then do
        putStrLn "Error: estudiante no encontrado"
        menuPrincipal mat
    else do
        nota <- fmap read (pedir "Nota:")
        manejarResultado mat (actualizarMateria mat (procesarNota cod nota))


-- ELIMINAR NOTA
eliminarNota :: Materia -> IO ()
eliminarNota mat = do
    cod <- pedir "Codigo:"

    let existe = any (\e -> codigo e == cod) (estudiantes mat)

    if not existe then do
        putStrLn "Error: estudiante no encontrado"
        menuPrincipal mat
    else do
        nota <- fmap read (pedir "Nota a eliminar:")
        manejarResultado mat (actualizarMateria mat (procesarEliminar cod nota))


-- MODIFICAR NOTA
modificarNota :: Materia -> IO ()
modificarNota mat = do
    cod <- pedir "Codigo:"

    let existe = any (\e -> codigo e == cod) (estudiantes mat)

    if not existe then do
        putStrLn "Error: estudiante no encontrado"
        menuPrincipal mat
    else do
        vieja <- fmap read (pedir "Nota actual:")
        nueva <- fmap read (pedir "Nueva nota:")
        manejarResultado mat (actualizarMateria mat (procesarModificar cod vieja nueva))


-- REPORTES
verReportes :: Materia -> IO ()
verReportes mat = do
    putStrLn (reporteMateria mat)

    putStrLn "\nPromedio materia:"
    print (promedioMateria mat)

    menuPrincipal mat


-- RANKING
verRanking :: Materia -> IO ()
verRanking mat = do
    putStrLn "\nRANKING:"
    let r = rankingEstudiantes mat

    if null r
        then putStrLn "No hay datos"
        else mapM_ (\(p,n) -> putStrLn (n ++ " -> " ++ show p)) r

    menuPrincipal mat


-- SUBMENU
submenuAvanzado :: Materia -> IO ()
submenuAvanzado mat = do
    putStrLn "\nFUNCIONES AVANZADAS"
    putStrLn "1. Estudiantes aprobados"
    putStrLn "2. Estudiantes reprobados"
    putStrLn "3. Nombres aprobados"
    putStrLn "4. Tabla de promedios"
    putStrLn "5. Calificaciones validas"
    putStrLn "6. Volver"

    op <- getLine

    case op of
        "1" -> do
            let r = estudiantesAprobados mat
            if null r then putStrLn "No hay estudiantes aprobados" else print r
            submenuAvanzado mat

        "2" -> do
            let r = estudiantesReprobados mat
            if null r then putStrLn "No hay estudiantes reprobados" else print r
            submenuAvanzado mat

        "3" -> do
            let r = nombresAprobados mat
            if null r then putStrLn "No hay aprobados" else print r
            submenuAvanzado mat

        "4" -> do
            let r = tablaPromedios mat
            if null r then putStrLn "No hay datos" else print r
            submenuAvanzado mat

        "5" -> do
            if null (estudiantes mat)
                then putStrLn "No hay estudiantes"
                else mapM_ (\e -> print (nombreEst e, calificacionesValidas e)) (estudiantes mat)
            submenuAvanzado mat

        "6" -> menuPrincipal mat

        _ -> submenuAvanzado mat


-- HELPERS

pedir :: String -> IO String
pedir msg = putStrLn msg >> getLine


manejarResultado :: Materia -> Either [String] Materia -> IO ()
manejarResultado mat resultado =
    case resultado of
        Left errs -> do
            mapM_ putStrLn errs
            menuPrincipal mat

        Right nueva -> do
            putStrLn "Operacion exitosa"
            menuPrincipal nueva