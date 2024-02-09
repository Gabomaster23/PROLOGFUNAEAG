-- Definimos un tipo de dato para representar las calificaciones
data Calificacion = Excelente | Notable | Bueno | Suficiente | Insuficiente deriving (Show)

-- Función que asigna una calificación a una nota
calificarNota :: Int -> Calificacion
calificarNota nota
    | nota >= 95 && nota <= 100 = Excelente
    | nota >= 85 && nota <= 94 = Notable
    | nota >= 75 && nota <= 84 = Bueno
    | nota >= 70 && nota <= 74 = Suficiente
    | otherwise = Insuficiente

-- Función que recibe una lista de calificaciones y devuelve la lista de calificaciones correspondientes
calificaciones :: [Int] -> [Calificacion]
calificaciones notas = map calificarNota notas

-- Función principal para probar el código
main :: IO ()
main = do
    let listaCalificaciones = [100, 90, 80, 73, 60]
    print $ calificaciones listaCalificaciones
