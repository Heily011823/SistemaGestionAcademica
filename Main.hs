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
    putStrLn "1. Gestion de Estudiantes (Agregar)"
    putStrLn "2. Ver Reportes"
    putStrLn "3. Ver Ranking Academico"
    putStrLn "4. Salir"
    putStr "\nSeleccione una opcion: "
    
    opcion <- getLine

    case opcion of

        --  AGREGAR ESTUDIANTE 
        "1" -> do
            putStrLn "Ingrese codigo del estudiante:"
            cod <- getLine

            putStrLn "Ingrese nombre del estudiante:"
            nombre <- getLine

            
            let nuevo = Estudiante cod nombre [] []

            let existe = any (\e -> codigo e == cod) (estudiantes mat)

            if existe then do
                putStrLn "Error: ya existe un estudiante con ese codigo"
                menuPrincipal mat
            else do
                let nuevaMateria = mat { estudiantes = nuevo : estudiantes mat }
                putStrLn "Estudiante agregado correctamente"
                menuPrincipal nuevaMateria


        -- REPORTES
        "2" -> do
            putStrLn "\nREPORTE DE MATERIA"
            putStrLn (reporteMateria mat)

            putStrLn "\nPromedio de la materia:"
            print (promedioMateria mat)

            putStrLn "\nEstudiantes aprobados:"
            print (nombresAprobados mat)

            putStrLn "\nTabla de promedios:"
            print (tablaPromedios mat)

            menuPrincipal mat


        -- RANKING
        "3" -> do
            let rank = rankingEstudiantes mat
            putStrLn "\nRANKING DE ESTUDIANTES (Mayor a menor)"
            print rank
            menuPrincipal mat


        -- SALIR
        "4" -> putStrLn "Saliendo del sistema..."


        -- ERROR
        _ -> do
            putStrLn "Opcion no valida."
            menuPrincipal mat