-- Calcula el módulo de un vector
moduloDelVector :: Floating a => [a] -> a
moduloDelVector = sqrt . sum . map (^2)

main :: IO ()
main = do
    let miVector = [3.0, 4.0]
    putStrLn $ "El módulo del vector " ++ show miVector ++ " es " ++ show (moduloDelVector miVector)