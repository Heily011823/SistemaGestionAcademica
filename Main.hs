module Main where
import Tipos         
import FuncionesBasicas 
import Validaciones  
import Reportes      
import Arbol        
import OrdenSuperior  

-- MAIN
main :: IO ()
main = do
    putStrLn "SISTEMA DE GESTION ACADEMICA"
    let materiaInicial = Materia "Paradigmas" 4 [] 
    menuPrincipal materiaInicial


-- MENU PRINCIPAL
menuPrincipal :: Materia -> IO ()
menuPrincipal mat = do
    putStrLn "\nMENU PRINCIPAL"
    putStrLn "1. Agregar estudiante"
    putStrLn "2. Agregar calificacion a estudiante"
    putStrLn "3. Ver reportes"
    putStrLn "4. Ver ranking"
    putStrLn "5. Salir"
    putStr "\nSeleccione una opcion: "
    
    opcion <- getLine

    case opcion of

        --  AGREGAR ESTUDIANTE
        "1" -> do
            putStrLn "Ingrese codigo:"
            cod <- getLine

            putStrLn "Ingrese nombre:"
            nombre <- getLine

            let nuevo = Estudiante cod nombre [] []

            let existe = any (\e -> codigo e == cod) (estudiantes mat)

            if existe then do
                putStrLn "Error: codigo duplicado"
                menuPrincipal mat
            else do
                let nuevaMateria = mat { estudiantes = nuevo : estudiantes mat }
                putStrLn "Estudiante agregado"
                menuPrincipal nuevaMateria


        -- AGREGAR CALIFICACION
        "2" -> do
            putStrLn "Ingrese codigo del estudiante:"
            cod <- getLine

            putStrLn "Ingrese nota:"
            notaStr <- getLine
            let nota = read notaStr :: Double

            let nuevaLista = map (agregarNotaAEstudiante cod nota) (estudiantes mat)

            let nuevaMateria = mat { estudiantes = nuevaLista }

            putStrLn "Proceso completado"
            menuPrincipal nuevaMateria


        -- REPORTES
        "3" -> do
            putStrLn (reporteMateria mat)

            putStrLn "\nPromedio de la materia:"
            print (promedioMateria mat)

            menuPrincipal mat


        -- RANKING
        "4" -> do
            print (rankingEstudiantes mat)
            menuPrincipal mat


        -- SALIR
        "5" -> putStrLn "Saliendo..."


        _ -> do
            putStrLn "Opcion invalida"
            menuPrincipal mat



-- usa Either y historial
agregarNotaAEstudiante :: String -> Double -> Estudiante -> Estudiante
agregarNotaAEstudiante cod nota est
    | codigo est /= cod = est
    | otherwise =
        case agregarCalificacion nota est of
            Left err -> est  -- no cambia si hay error
            Right nuevoEst ->
                nuevoEst {
                    historial = historial est ++ [AgregarNota nota]
                }