
--ej 1
longitud :: [t] -> Int
logitud [] = 0 
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

--ej 2.a)
pertenece ::(Eq t) => t-> [t]-> Bool 
pertenece _ [] = False 
pertenece x (y:ys) | x == y = True 
                   | otherwise = pertenece x ys

--ej 2
-- todosIguales :: (Eq t ) => 