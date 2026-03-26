module Tipos where

-- 
-- PARTE 1 - TIPOS DE DATOS
-- 

-- Tipo de nota
data Nota = Reprobado | Aprobado | Notable | Excelente
  deriving (Show, Eq, Ord)

-- Clasifica una nota
clasificar :: Double -> Maybe Nota
clasificar n
  | n < 0 || n > 5 = Nothing
  | n < 3.0        = Just Reprobado
  | n < 3.5        = Just Aprobado
  | n < 4.5        = Just Notable
  | otherwise      = Just Excelente


-- 
-- PARTE 9 - HISTORIAL
-- 

data Cambio
  = AgregarNota Double
  | EliminarNota Double
  | ModificarNota Double Double
  deriving (Show, Eq)


-- ESTUDIANTE 
data Estudiante = Estudiante
  { codigo :: String
  , nombreEst :: String
  , califs :: [Double]
  , historial :: [Cambio]
  } deriving (Show, Eq)


-- MATERIA
data Materia = Materia
  { nombreMat :: String
  , creditos :: Int
  , estudiantes :: [Estudiante]
  } deriving (Show, Eq)