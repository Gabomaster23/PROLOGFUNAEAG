import qualified Data.Map as Map

applyDescuento :: Double -> Double -> Double
applyDescuento precio descuento = precio - precio * descuento / 100

applyIVA :: Double -> Double -> Double
applyIVA precio porcentaje = precio + precio * porcentaje / 100

precioCesta :: Map.Map Double Double -> (Double -> Double -> Double) -> Double
precioCesta cesta funcion = Map.foldlWithKey' (\acc precio descuento -> acc + funcion precio descuento) 0 cesta

main :: IO ()
main = do
  let cesta = Map.fromList [(1000, 20), (500, 10), (100, 1)]
  putStrLn $ "El precio de la cesta tras aplicar los descuentos es: " ++ show (precioCesta cesta applyDescuento)
  putStrLn $ "El precio de la cesta tras aplicar el IVA es: " ++ show (precioCesta cesta applyIVA)
