
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
reverso (x:xs) | longitud (x:xs) == 1 = x:[]
               | otherwise = (reverso xs) ++ (x:[])

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