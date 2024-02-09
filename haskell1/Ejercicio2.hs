-- Función que aplica una función a todos los elementos de una lista.
aplicaFuncionLista :: (a -> b) -> [a] -> [b]
aplicaFuncionLista _ [] = [] -- Caso base: lista vacía
aplicaFuncionLista f (x:xs) = f x : aplicaFuncionLista f xs -- Aplicar la función al primer elemento y recursivamente al resto de la lista

-- Función cuadrado
cuadrado :: Num a => a -> a
cuadrado n = n * n

main :: IO ()
main = do
    print $ aplicaFuncionLista cuadrado [1, 2, 3, 4]