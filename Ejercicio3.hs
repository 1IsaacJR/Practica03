--Ejercicio3


--Ver si funcionan las funciones  
{-
{-FUNCIONA-}
--Funcion para identificar el numero maximo de primos a multiplicar
indenPrimos:: [Integer] -> Integer
indenPrimos [] = 0
indenPrimos (x:xs)
    | x == 0 = 0
    | otherwise = 1 + indenPrimos xs

-- Función que verifica si un número es primo
esPrimo :: Integer -> Bool
esPrimo n
  | n <= 1    = False
  | n == 2     = True
  | n `mod` 2 == 0 = False
  | otherwise = True
  
-- Función que devuelve una lista de números primos menores o iguales a un número dado
listaDePrimos :: Integer -> [Integer]
listaDePrimos n = filter esPrimo [2..n]

--Funcion para generar el minimo de primos despues de saber cual es el maximo
generarPrimos:: [Integer] -> [Integer]
generarPrimos [] = [] 
generarPrimos (x:xs) = listaDePrimos (indenPrimos (x:xs))
-}

--Funcion Prop definida anteriormente 
data Prop = Var String
          | No Prop
          | Prop :& Prop
          | Prop :| Prop
          | Prop :/ Prop
          | Parenizq Prop
          | Parender Prop 
  deriving (Eq, Show)

enumProp :: Prop -> [Integer] -> [Integer]
enumProp _ [] = []  -- Caso base: si la lista está vacía, devolvemos una lista vacía
enumProp (Var _) (x:xs) = x : enumProp (Var "") xs  
enumProp (No p) (x:xs) = x : enumProp p xs  
enumProp (p :& q) (x:xs) = enumProp p xs ++ enumProp q xs 
enumProp (p :| q) (x:xs) = enumProp p xs ++ enumProp q xs 
enumProp (p :/ q) (x:xs) = enumProp p xs ++ enumProp q xs 
enumProp (Parenizq p) (x:xs) = enumProp p xs
enumProp (Parender p) (x:xs) = enumProp p xs


main:: IO()
main  = do
{-No fuciona -}
    let prop1 = Var "a" :& Var "b"  -- Conjunción de dos proposiciones (Var "a" y Var "b")
    let prop2 = No (Var "c") :& Var "d"  -- Negación de (Var "c") y conjunción con (Var "d")
    let prop3 = Parenizq (Var "e") :& Parender (Var "f")  -- Paréntesis izquierdo y derecho

    -- Números primos para probar
    let primes = [2, 3, 5, 7, 11, 13]

    -- Evaluamos las proposiciones con la lista de números primos
    let result1 = enumProp prop1 primes
    let result2 = enumProp prop2 primes
    let result3 = enumProp prop3 primes

    -- Mostramos los resultados
    print ("Resultado para prop1 (Var 'a' :& Var 'b'): " ++ show result1)
    print ("Resultado para prop2 (No (Var 'c') :& Var 'd'): " ++ show result2)
    print ("Resultado para prop3 (Parenizq (Var 'e') :& Parender (Var 'f')): " ++ show result3)


--        let lista = [1, 2, 4, 5, 8, 9, 10, 11, 90, 56]
--	print (generarPrimos lista)

