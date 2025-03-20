--Ejercicio 1

--Funcion que dado un entero devuelva una lista de factores primos
factoresPrimos:: Integer -> [Integer] -> [Integer]
factoresPrimos 1 (x:xs) = [] 
factoresPrimos n (x:xs) 
	       | n `mod` x == 0 = x:factoresPrimos (n `div` x)(x:xs)
	       | otherwise  = factoresPrimos n xs 

--Funcion principal para obtener los factores primos de un numero
factores :: Integer -> [Integer]
factores n = factoresPrimos n [2..n]

-- Función que verifica si un número es primo
esPrimo :: Integer -> Bool
esPrimo n
  | n <= 1    = False
  | n == 2     = True
  | n `mod` 2 == 0 = False
  | otherwise = True

--  | otherwise = not (any (\x -> n `mod` x == 0) [3..(floor . sqrt . fromIntegral) n])

-- Función que devuelve una lista de números primos menores o iguales a un número dado
listaDePrimos :: Integer -> [Integer]
listaDePrimos n = filter esPrimo [2..n]

-- Metodo main
main :: IO ()
main = do
--    let n = 10
--    print (factoresPrimos n)
      print (listaDePrimos 10)  -- [2, 3, 5, 7]	
      print (factores 10)	

