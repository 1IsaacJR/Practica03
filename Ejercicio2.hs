-- Ejercicio02

{-
Identificar que si existen proposiciones diferentes se puedan enumerar
como por ejemplo si tengo "p" y "q" que se enumere "p" y "q",
que no lo tome como un unico valor

--Funcion Prop definida anteriormente 
data Prop = Var String
 | No Prop
 | Prop :& Prop
 | Prop :| Prop
 | Prop :/ Prop
 | Prop Parenizq Prop --(
 | Prop Parender Prop --)
deriving( Eq, Show )

--Prueba1: Definir un tipo de valor que se le asigne a cada valor
--Tipo de dato prop
data Prop = Var String
 | No Prop
 | Prop :& Prop
 | Prop :| Prop
 | Prop :/ Prop
 | Prop ")" Prop
 | Prop "(" Prop
deriving(Eq, Show)

IDEA DE IMPLEMENTACION DEL 2
-Funcion que devuelva una lisa de variables reconocidas en la formula:
	 ejemplo: "(p -> q) v (neg p -> neg q)" = ["(","p","->"..."q"]
enumProp :: Prop -> [Integer] -> [Vars]
- Generamos una lista de primos dependiendo el numero de ariables de la lista anterior
  	 ejemplo:  ["(","p","->"..."q"] = 13(variables) = [2,3,7..](generamos la lista de primos)
- Funcion que genere duplas entre la lista de primos posibles y el numero de primos de cada variable
  	 ejemplo:  "(p -> q) v (neg p -> neg q)"(de esta expresion obtuvimos los primos) = [7,29,7,29,3,2..] 
	 	        (lista obtenida de la enumeracion de primos)[7,29,7,29,3,2..] * (lista del numero total de primos a multiplicar)[2,3,7..]
			
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

--Funcion donde se multiplica por cada primo posible de la lista de primos

-- Función que verifica si un número es primo
esPrimo :: Integer -> Bool
esPrimo n
  | n <= 1    = False
  | n == 2     = True
  | n `mod` 2 == 0 = False
  | otherwise = True
  
-- Función que devuelve una lista de números primos menores o iguales a un número dado
listaDePrimos :: Integer -> [Integer]
--Utilizamos la funcion "filter" que facilita tomar una lista con un indice i hasta un indice i + 1
listaDePrimos n = filter esPrimo [2..n]

--Funcion para generar el minimo de primos despues de saber cual es el maximo
generarPrimos:: Integer -> [Integer]
generarPrimos n = listaDePrimos n

--Funcion para identificar el numero maximo de primos a multiplicar
indenPrimos:: [Integer] -> Integer
indenPrimos [] = 0
indenPrimos (x:xs)
    | x == 0 = 0
    | otherwise = 1 + indenPrimos xs

