module Main where
import Tipos         
import FuncionesBasicas 
import Validaciones  
import Reportes      
import Arbol        

-- Estructura principal del Programa 
main :: IO ()
main = do
    putStrLn "SISTEMA DE GESTION ACADEMICA"

    -- Iniciamos con una materia vacía o de ejemplo
    let materiaInicial = Materia "Paradigmas" 4 [] 
    menuPrincipal materiaInicial

menuPrincipal :: Materia -> IO ()
menuPrincipal mat = do
    putStrLn "\nMENU PRINCIPAL"
    putStrLn "1. Gestion de Estudiantes (Agregar/Validar)" 
    putStrLn "2. Ver Reportes (Estudiante/Materia)"        
    putStrLn "3. Ver Ranking Academico (Arbol)"            
    putStrLn "4. Historial de Cambios"                     
    putStrLn "5. Configurar Ponderacion"                   
    putStrLn "6. Salir"
    putStr "\nSeleccione una opcion: "
    
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese nombre del estudiante:"
            nombre <- getLine
            -- Aquí se llamaría a 'validarEstudiante' 
            putStrLn "Estudiante procesado."
            menuPrincipal mat

        "2" -> do
            -- Parte 5: Reportes
            putStrLn $ reporteMateria mat
            menuPrincipal mat

        "3" -> do
            -- Parte 6: Árbol y Ranking
            let rank = rankingEstudiantes mat
            putStrLn "RANKING DE NOTAS"
            print rank
            menuPrincipal mat

        "4" -> do
            -- Parte 9: Historial 
            putStrLn "Mostrando historial de cambios..."
            menuPrincipal mat

        "5" -> do
            -- Parte 11: Ponderación
            putStrLn "Configurando promedio ponderado..."
            menuPrincipal mat

        "6" -> putStrLn "Saliendo del sistema. ¡Hasta luego!"

        _ -> do
            putStrLn "Opcion no valida."
            menuPrincipal mat