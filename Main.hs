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
            codInput <- getLine

            case validarCodigo codInput of
                Left err -> do
                    putStrLn err
                    menuPrincipal mat

                Right cod -> do
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


        --  AGREGAR CALIFICACION 
        "2" -> do
            putStrLn "Ingrese codigo del estudiante:"
            cod <- getLine

            putStrLn "Ingrese nota:"
            notaStr <- getLine
            let nota = read notaStr :: Double

            -- Verificar si existe el estudiante
            let existe = any (\e -> codigo e == cod) (estudiantes mat)

            if not existe then do
                putStrLn "Error: estudiante no encontrado"
                menuPrincipal mat
            else do
                let resultado = map (procesarNota cod nota) (estudiantes mat)

                let errores = [e | Left e <- resultado]

                if not (null errores) then do
                    putStrLn "Error al agregar nota:"
                    mapM_ putStrLn errores
                    menuPrincipal mat
                else do
                    let nuevosEst = [e | Right e <- resultado]
                    let nuevaMateria = mat { estudiantes = nuevosEst }

                    putStrLn "Calificacion agregada correctamente"
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


        -- ERROR
        _ -> do
            putStrLn "Opcion invalida"
            menuPrincipal mat



-- FUNCION Either y historial
procesarNota :: String -> Double -> Estudiante -> Either String Estudiante
procesarNota cod nota est
    | codigo est /= cod = Right est
    | otherwise =
        case agregarCalificacion nota est of
            Left err -> Left err
            Right nuevoEst ->
                Right nuevoEst {
                    historial = historial est ++ [AgregarNota nota]
                }