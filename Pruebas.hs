import Data.List (nub)


--Aplicar esta funcion en las otras

--Funcion para identificar el numero maximo de primos a multiplicar
indenPrimos1:: [Integer] -> Integer
indenPrimos1 [] = 0
indenPrimos1 (x:xs)
    | x == 0 = 0
    | otherwise = 1 + indenPrimos1 xs

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

-- Función auxiliar que busca los primeros "n" primos dado un numero "n"
primosAux :: Integer -> Integer -> [Integer] -> [Integer]
primosAux _ 0 p = p  -- Caso base: si ya tenemos 'n' primos, devolvemos la lista
primosAux m n p
  | esPrimo m = primosAux (m + 1) (n - 1) (p ++ [m])  -- Si es primo, lo agregamos
  | otherwise = primosAux (m + 1) n p  -- Si no es primo, continuamos



{-Esta funcion es para cuando tengamos la lista de primos generados por las variables-}

--Funcion para generar el minimo de primos despues de saber cual es el maximo
generarPrimos:: [Integer] -> [Integer]
generarPrimos [] = [] 
generarPrimos (x:xs) = listaDePrimos (indenPrimos1 (x:xs))



--Funcion Prop definida anteriormente con dos varaibles mas:"Parenizq Prop" y "Parender Prop"
data Prop = Var String
          | No Prop
          | Prop :& Prop
          | Prop :| Prop
          | Prop :/ Prop
          | Parenizq Prop --parentesis izquierdo
          | Parender Prop --parentesis derecho
  deriving (Eq, Show)

-- Función para identificar variables y operadores para una lista de Strings
idenVarYOp:: Prop -> [String]
idenVarYOp (Var v) = [v]  -- Solo una variable
idenVarYOp (No p) = ["No"] ++ idenVarYOp p
idenVarYOp (p :& q) = [":&"] ++ idenVarYOp p ++ idenVarYOp q
idenVarYOp (p :| q) = [":|"] ++ idenVarYOp p ++ idenVarYOp q  
idenVarYOp (p :/ q) = [":/"] ++ idenVarYOp p ++ idenVarYOp q  
idenVarYOp (Parenizq p) = ["Parenizq"] ++ idenVarYOp p  
idenVarYOp (Parender p) = ["Parender"] ++ idenVarYOp p  


-- Función para identificar el número de primos a multiplicar dependiendo del número de símbolos (variables y operaciones)
indenPrimos :: [String] -> Integer
indenPrimos [] = 0
indenPrimos (x:xs)
    | esOperacion x = 1 + indenPrimos xs  -- Cuenta si es operación o variable
    | otherwise = indenPrimos xs  -- No cuenta otros casos
--Utilizamos where para definir una funcion si dentro de la lista hay varaibles y operaciones 
    where
-- Función que verifica si la expresión es una operación
    esOperacion :: String -> Bool
    esOperacion x = not (x `elem` ["No", ":&", ":|", ":/"])
    esOperacion x
        | x == "No" = True
	| x == ":&" = True   
    	| x == ":|" = True
	| x == ":/" = True   -- Es una operación

-- Función que evalúa ambas funciones para devolver la lista de primos generada por la Proposicion logica
evaluarExpresion :: Prop -> Integer
evaluarExpresion expr = indenPrimos(idenVarYOp expr)

--Funcion evaluamos un entero para una lista de valoes
listaDePrimos2 ::  Prop -> [Integer]
listaDePrimos2 expr = primosAux 2 (evaluarExpresion expr) []


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






-- Función principal
main :: IO ()
main = do
    let exp = Parenizq (Var "a" :& Parender (Var "b" :| Var "a"))   -- Un ejemplo donde "a" aparece dos veces
    print (nub (idenVarYOp exp))  -- Imprime las variables y los operadores sin duplicados
    print (evaluarExpresion exp)
    print (listaDePrimos2 exp)
