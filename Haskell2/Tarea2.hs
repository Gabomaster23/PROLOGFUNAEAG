module TareaDiferente where

import Data.Char (toUpper)

-- Parte 1 de la tarea: Funciones matemáticas

-- Funciones para calcular funciones matemáticas como seno, coseno, tangente, exponencial y logaritmo neperiano.
calcularSeno :: Double -> Double
calcularSeno x = sin x

calcularCoseno :: Double -> Double
calcularCoseno x = cos x

calcularTangente :: Double -> Double
calcularTangente x = tan x

calcularExponencial :: Double -> Double
calcularExponencial x = exp x

calcularLogaritmoNeperiano :: Double -> Double
calcularLogaritmoNeperiano x = log x

-- Función que genera una lista de valores calculados aplicando una función matemática a enteros desde 1 hasta n.
calcularFuncion :: (Double -> Double) -> Int -> [(Int, Double)]
calcularFuncion f n = [(x, f $ fromIntegral x) | x <- [1..n]]

-- Parte 2 de la tarea: Funciones de filtrado

-- Función para filtrar elementos de una lista según una condición booleana.
filtraLista :: (a -> Bool) -> [a] -> [a]
filtraLista _ [] = []
filtraLista f (x:xs)
    | f x       = x : filtraLista f xs
    | otherwise = filtraLista f xs

-- Función que verifica si un número es par.
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

-- Parte 3 de la tarea: Conversión de calificaciones

-- Función para convertir una calificación numérica en un texto descriptivo.
convertirCalificaciones :: Double -> String
convertirCalificaciones nota
    | nota >= 95 = "Muy bien"
    | nota >= 85 = "Bastante bien"
    | nota >= 75 = "Aceptable"
    | nota >= 70 = "Suficiente"
    | otherwise  = "Poco satisfactorio"

-- Función que aplica la conversión de calificaciones a una lista de calificaciones.
calificaciones :: [Double] -> [String]
calificaciones = map convertirCalificaciones

-- Parte 4 de la tarea: Manipulación de diccionarios de calificaciones

-- Función para convertir las calificaciones de un diccionario a texto y a mayúsculas.
calificacionesDiccionario :: [(String, Double)] -> [(String, String)]
calificacionesDiccionario = map (\(asignatura, nota) -> (map toUpper asignatura, convertirCalificaciones nota))

-- Parte 5 de la tarea: Cálculo de precios de inmuebles

-- Definición de tipos para representar inmuebles y presupuestos.
type Inmueble = (Int, Int, Int, Bool, Char)
type Presupuesto = Int

-- Función para calcular el precio de un inmueble según sus características y zona.
precioInmueble :: Inmueble -> Float
precioInmueble (año, metros, habitaciones, garaje, zona)
    | zona == 'A' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100)
    | zona == 'B' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100) * 1.5
    | otherwise = 0.0

-- Función que filtra una lista de inmuebles según un presupuesto máximo.
buscarInmuebles :: [Inmueble] -> Presupuesto -> [Inmueble]
buscarInmuebles inmuebles presupuesto = filter (\x -> precioInmueble x <= fromIntegral presupuesto) inmuebles
