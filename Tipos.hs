module Tipos where

data Nota = Reprobado | Aprobado | Notable | Excelente deriving (Show, Eq, Ord)

clasificar :: Double -> Maybe Nota
clasificar n
  | n < 0 || n > 5 = Nothing
  | n < 3 = Just Reprobado
  | n < 4 = Just Aprobado
  | n < 4.5 = Just Notable
  | otherwise = Just Excelente

data Cambio = AgregarNota Double | EliminarNota Double | ModificarNota Double Double deriving (Show)

data Estudiante = Estudiante
  { codigo :: String
  , nombreEst :: String
  , califs :: [Double]
  , historial :: [Cambio]   
  } deriving (Show, Eq)

data Materia = Materia
  { nombreMat :: String
  , creditos :: Int
  , estudiantes :: [Estudiante]
  } deriving (Show)