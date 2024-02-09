import Data.Map.Strict (Map, fromListWith)

-- Función que toma una cadena de texto y devuelve un mapa donde las claves son las palabras
-- y los valores son las longitudes de esas palabras.
longitudesDePalabras :: String -> Map String Int
longitudesDePalabras = fromListWith max . map (\x -> (x, length x)) . words

main :: IO ()
main = do
    -- Definición de la cadena de texto que queremos procesar.
    let oracion = "Esta es una oración de ejemplo para probar la función."
    -- Imprime el resultado de aplicar la función longitudesDePalabras a la oración.
    print $ longitudesDePalabras oracion
