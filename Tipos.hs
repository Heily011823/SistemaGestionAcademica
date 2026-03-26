module Tipos where


-- PARTE 1 - TIPOS DE DATOS


-- Tipo de nota segun el rendimiento
data Nota = Reprobado | Aprobado | Notable | Excelente
  deriving (Show, Eq, Ord)

-- Clasifica una nota numerica a su categoria
-- Si esta fuera de rango devuelve Nothing
clasificar :: Double -> Maybe Nota
clasificar n
  | n < 0 || n > 5 = Nothing
  | n < 3.0        = Just Reprobado
  | n < 3.5        = Just Aprobado
  | n < 4.5        = Just Notable
  | otherwise      = Just Excelente

-- Representa un estudiante con sus datos
data Estudiante = Estudiante
  { codigo :: String
  , nombreEst :: String
  , califs :: [Double]
  } deriving (Show, Eq)

-- Representa una materia con estudiantes inscritos
data Materia = Materia
  { nombreMat :: String
  , creditos :: Int
  , estudiantes :: [Estudiante]
  } deriving (Show, Eq)



-- PARTE 4 - VALIDACIONES (BASICO)


-- Verifica que la nota este entre 0 y 5
validarCalificacion :: Double -> Either String Double
validarCalificacion n
  | n < 0 || n > 5 = Left "Calificacion invalida (0 a 5)"
  | otherwise      = Right n

-- Valida que el estudiante tenga datos correctos
validarEstudiante :: Estudiante -> Either String Estudiante
validarEstudiante e
  | null (codigo e) = Left "Codigo vacio"
  | null (nombreEst e) = Left "Nombre vacio"
  | length (califs e) > 5 = Left "Maximo 5 calificaciones"
  | otherwise = Right e

-- Agrega una calificacion si es valida y no supera el limite
agregarCalificacion :: Double -> Estudiante -> Either String Estudiante
agregarCalificacion n e = do
  notaValida <- validarCalificacion n
  if length (califs e) >= 5
    then Left "No se pueden agregar mas de 5 notas"
    else Right e { califs = califs e ++ [notaValida] }



-- SOPORTE (RECUSION + MAYBE)


-- Busca un estudiante por codigo
-- Devuelve Just estudiante o Nothing si no existe
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante _ [] = Nothing
buscarEstudiante cod (e:es)
  | codigo e == cod = Just e
  | otherwise       = buscarEstudiante cod es



-- EXTRA (OPCIONAL)


-- Busca la posicion de un estudiante en la lista
-- Devuelve Just posicion o Nothing si no lo encuentra
buscarIndiceEstudiante :: String -> [Estudiante] -> Maybe Int
buscarIndiceEstudiante cod xs = go xs 0
  where
    go [] _ = Nothing
    go (e:es) i
      | codigo e == cod = Just i
      | otherwise       = go es (i + 1)