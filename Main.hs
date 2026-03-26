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
    putStrLn "7. Salir"
    opcion <- getLine

    case opcion of

        -- AGREGAR ESTUDIANTE
        "1" -> do
            putStrLn "Codigo:"
            codInput <- getLine

            case validarCodigo codInput of
                Left err -> print err >> menuPrincipal mat

                Right cod -> do
                    putStrLn "Nombre:"
                    nombre <- getLine

                    let existe = any (\e -> codigo e == cod) (estudiantes mat)

                    if existe then
                        putStrLn "Codigo duplicado" >> menuPrincipal mat
                    else do
                        let nuevo = Estudiante cod nombre [] []
                        menuPrincipal mat { estudiantes = nuevo : estudiantes mat }


        -- AGREGAR NOTA
        "2" -> do
            cod <- pedir "Codigo:"
            nota <- fmap read (pedir "Nota:")

            manejarResultado mat (actualizarMateria mat (procesarNota cod nota))


        -- ELIMINAR
        "3" -> do
            cod <- pedir "Codigo:"
            nota <- fmap read (pedir "Nota a eliminar:")

            manejarResultado mat (actualizarMateria mat (procesarEliminar cod nota))


        -- MODIFICAR
        "4" -> do
            cod <- pedir "Codigo:"
            vieja <- fmap read (pedir "Nota actual:")
            nueva <- fmap read (pedir "Nueva nota:")

            manejarResultado mat (actualizarMateria mat (procesarModificar cod vieja nueva))


        -- REPORTES
        "5" -> do
            putStrLn (reporteMateria mat)
            print (promedioMateria mat)
            menuPrincipal mat


        -- RANKING
        "6" -> do
            print (rankingEstudiantes mat)
            menuPrincipal mat


        -- SALIR
        "7" -> putStrLn "Saliendo..."


        _ -> menuPrincipal mat


-- helpers de IO
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