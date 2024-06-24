

--ej 1
longitud :: [t] -> Int
longitud [] = 0 
longitud (x:xs) = 1 + longitud xs

--ej b
-- ultimo :: [t] -> t
-- ultimo (x:xs) | longitud(x:xs) == 1 = x
--               | otherwise = ultimo xs

ultimo :: [t] -> t --No listas vacias
ultimo (x:xs) |longitud (x:xs) == 1 = x
              |longitud (x:xs) > 1 = ultimo xs


--ej c  
principio :: [t] -> [t]
principio (x:xs) | longitud (x:xs) == 2 = x:[]
                 | otherwise = x: principio xs
--ej d 
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) | longitud (x:xs) == 1 = [x]
               | otherwise = reverso xs ++ [x]

lista = [1, 2, 3]


--ej 2.a)
pertenece ::(Eq t) => t-> [t]-> Bool 
pertenece _ [] = False 
pertenece x (y:ys) | x == y = True 
                   | otherwise = pertenece x ys

--ej 2
todosIguales :: (Eq t ) => [t] -> Bool 
todosIguales [] = False
todosIguales (x:xs) | longitud (x:xs) == 1 = True 
                    | otherwise = pertenece x xs && todosIguales xs
-- 2.3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos (x:xs) | longitud (x:xs) == 1 = True
                      | otherwise = not(pertenece x xs) && todosDistintos xs 

-- 2.4 
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | longitud (x:xs) <= 1 = False
                    | pertenece x xs = True
                    | otherwise = hayRepetidos xs

--2.5 
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) | n == x = xs -- no hay recursion, entonces solo se ejecuta una vez
                | otherwise = x:quitar n xs

--2.6 
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n (x:xs) | n == x = quitarTodos n xs
                     | otherwise = x: quitarTodos n xs 

--2.7 
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos (quitarTodos x xs)
                         | otherwise = x: eliminarRepetidos xs

--2.8
mismosElementos :: (Eq t) => [t] -> [t] ->Bool 
mismosElementos [] [] = True 
mismosElementos _ [] = False 
mismosElementos [] _ = False 
mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) = mismosElementos (quitarTodos x xs) (quitarTodos x (y:ys))
                              | otherwise = False 

l1 = [1, 2, 3, 4]
l2 = [3,3, 5,6]

l3 =  [1, 2, 3, 4]

--2.9 
capicua :: (Eq t) => [t] -> Bool 
capicua [] = True
capicua lista  | lista == reverso lista = True
               | otherwise = False 

--listas de ejemplo 
a = ['a', 'b' , 'a']
b = ['a', 'b', 'c', 'd', 'f']

--Ejercicio 3
--3.1 
sumatoria :: [Integer] -> Integer
sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs

--3.2 
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs 


--3.3 
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:x1:xs) | x < x1 = maximo (x1:xs)
                 | otherwise = maximo (x:xs)

--3.4 
sumaN :: Integer -> [Integer] -> [Integer]
sumaN _ [] = []
sumaN n (x:xs) = x+n: sumaN n xs

--3.5 
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero [x] = [x + x]
sumarElPrimero (x:xs) = x + x :sumaN x xs

--3.6 
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo [x] = [x+x]
sumarElUltimo (x:xs) = sumaN (ultimo (x:xs)) (x:xs)

--3.7 
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x: pares xs
             | otherwise = pares xs 

--3.9
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | ((mod x n )== 0) = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

--3.9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar lista = minimo lista : ordenar (quitar (minimo lista) lista)

minimo :: [Integer] -> Integer
minimo [x] = x
minimo (x:y:xs) | x > y = minimo (y:xs) --y es menor a x, va guardando el menor 
                | otherwise = minimo (x:xs)

--4.0
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [y] = y: []
sacarBlancosRepetidos (x:y:xs) | x == ' ' && y == ' ' = sacarBlancosRepetidos (' ':xs) 
                               |otherwise = x: sacarBlancosRepetidos(y:xs)
--4.b 
contarPalabras :: [Char] -> Integer
contarPalabras l = contarPalabrasSBR (limpiarLista l)

contarPalabrasSBR :: [Char] -> Integer --Aux de contar palabras donde se sacaron los blancos repetidos
contarPalabrasSBR [] = 0
contarPalabrasSBR [x] = 1 --Porque la primer palabra no se cuenta, hay que compensar
contarPalabrasSBR (x:xs) | x == ' ' = 1  +  contarPalabrasSBR xs
                         | otherwise = contarPalabrasSBR xs

--c) -- dada una lista arma una nueva lista con las palabras de la lista original.
--[]++[1]++[2] == [1,2]

palabras::[Char] -> [[Char]]
palabras [] = []
palabras l = palabrasAux(limpiarLista l) []

--              Lista    Vacio
palabrasAux :: [Char] -> [Char] -> [[Char]]
palabrasAux [] palabra = [palabra]
palabrasAux (x:xs) palabra | x /= ' ' = palabrasAux xs (palabra ++ [x])
                           | otherwise = [palabra] ++ (palabrasAux xs [])


sacarPrimerBlancos :: [Char] -> [Char]
sacarPrimerBlancos [] = []
sacarPrimerBlancos (x:xs) | x == ' ' = sacarPrimerBlancos xs
                          | otherwise = (x:xs)

limpiarLista :: [Char] -> [Char] --Saca espacios innecesarios
limpiarLista [] = []
limpiarLista l = reverso(sacarPrimerBlancos(reverso(sacarPrimerBlancos(sacarBlancosRepetidos l))))

l4 = [' ','L','I','S','t','a','d','o']