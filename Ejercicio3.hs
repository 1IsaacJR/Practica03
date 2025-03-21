--Ejercicio3
--Referencia de la libreria (nub) Para eliminar tipos de datos repetidos
--https://hackage.haskell.org/package/universum-0.9.2/docs/Nub.html

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
listaDePrimos n = primosAux 2 n []


-- Función auxiliar que busca los primeros 'n' primos
primosAux :: Integer -> Integer -> [Integer] -> [Integer]
primosAux _ 0 p = p  -- Caso base: si ya tenemos 'n' primos, devolvemos la lista
primosAux m n p
  | esPrimo m = primosAux (m + 1) (n - 1) (p ++ [m])  -- Si es primo, lo agregamos
  | otherwise = primosAux (m + 1) n p  -- Si no es primo, continuamos



{-Esta funcion es para cuando tengamos la lista de primos generados por las variables-}

--Funcion para generar el minimo de primos despues de saber cual es el maximo
generarPrimos:: [Integer] -> [Integer]
generarPrimos [] = [] 
generarPrimos (x:xs) = listaDePrimos (indenPrimos (x:xs))

main:: IO()
main  = do

        let lista = [1, 2, 4, 5, 8, 9, 10, 11, 90, 56]
	print (generarPrimos lista)
	print (indenPrimos lista)
	print (indenPrimos lista)
	print (listaDePrimos 10)
-}
import Data.List (nub)

--Funcion Prop definida anteriormente 
data Prop = Var String
          | No Prop
          | Prop :& Prop
          | Prop :| Prop
          | Prop :/ Prop
          | Parenizq Prop
          | Parender Prop
  deriving (Eq, Show)

-- Función para identificar variables y operadores para una lista de Strings
idenVarYOp:: Prop -> [String]
idenVarYOp (Var v) = [v]  -- Solo una variable
idenVarYOp (No p) = ["No"] ++ idenVarYOp p  -- Operador "No"
idenVarYOp (p :& q) = [":&"] ++ idenVarYOp p ++ idenVarYOp q  -- Operador ":&"
idenVarYOp (p :| q) = [":|"] ++ idenVarYOp p ++ idenVarYOp q  -- Operador ":|"
idenVarYOp (p :/ q) = [":/"] ++ idenVarYOp p ++ idenVarYOp q  -- Operador ":/"
idenVarYOp (Parenizq p) = ["Parenizq"] ++ idenVarYOp p  -- Operador "Parenizq"
idenVarYOp (Parender p) = ["Parender"] ++ idenVarYOp p  -- Operador "Parender"

-- Función principal
main :: IO ()
main = do
    let exp = Parenizq (Var "a" :& Parender (Var "b" :| Var "a"))   -- Un ejemplo donde "a" aparece dos veces
    print (nub (idenVarYOp exp))  -- Imprime las variables y los operadores sin duplicados


{-
--Funcion que genera una tupla de lista de potencias y las multiplica
tuplaPotencia :: [Integer] -> [Integer] -> Integer
tuplaPotencia [] [] = 1  -- Base case: ambas listas vacías, se retorna 0
tuplaPotencia (x:xs) (y:ys) = ((x^y) * tuplaPotencia xs ys) --Multiplica las potencias de cada entrada de cada lista

main:: IO()
main  = do

        let lista1 = [2, 2, 2]
	let lista2 = [1, 2, 3]
	print (tuplaPotencia lista1 lista2)
-}

{-

--Funcion para pedir un primo para enumerar la variable de haskell y agregarlo a la enumeracion de primos
funPedirnum:: IO()-> String -> Integer  
funPedirnum putStrLn "Introduce un número:" = input <- getLine


main:: IO()
main  = do

        let lista1 = [2, 2, 2]
	let lista2 = [1, 2, 3]
	print (tuplaPotencia lista1 lista2)
-}



{-
--Funcion Prop definida anteriormente 
data Prop = Var String
          | No Prop
          | Prop :& Prop
          | Prop :| Prop
          | Prop :/ Prop
          | Parenizq Prop
          | Parender Prop 
  deriving (Eq, Show)

--Debe devolver una lista de Prop, de tal manera que haga duplas con primos
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

-}