
--1) Porcentaje de votos afirmativos
--porcentajeDeVotosAfirmativos :: [(String, String)] -> [Int] -> Int  -> Float
-- porcentajeDeVotosAfirmativos _ _ _ = 0

division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

f1 = [("Miguel","Marcos"),("Miriam","Julian"),("Sam","Mateo"),("Lucas","Pablo")]
v1 = [98,34,14,64] :: [Int]
--210 = 100% 
v2 = [0,304,0,74] :: [Int]
--378 = 100%

sumaVotosAfirmativos :: [(String, String)] -> [Int] -> Int 
sumaVotosAfirmativos [] [] = 0 
sumaVotosAfirmativos (x:xs) (y:ys) | xs /= [] = y + sumaVotosAfirmativos xs ys --va sumando elemento a elemento
                                   | otherwise = y

porcentajeDeVotosAfirmativos :: [(String, String)] -> [Int] ->Int -> Float
porcentajeDeVotosAfirmativos [] [] 0 = 100 --100% de afirmatividad porque no hubo votos en blanco
porcentajeDeVotosAfirmativos [] [] _ = 0 --no hubo votados pero si hubo votos, entonces no hay 100% de afirmatvidad (hay votos en blanco)
porcentajeDeVotosAfirmativos formula votos votosTotales = division(sumaVotosAfirmativos formula votos) votosTotales * 100

--punto 2
formulasInvalidas :: [(String, String)] -> Bool
formulasInvalidas [] = True --porque está vacía
formulasInvalidas (x:xs) | not(sonIguales x) = not(perteneceLista x xs) && formulasInvalidas xs 

--FORMULAS INVALIDAS 
l1 = [("Miguel","Marcos"),("Miriam","Julian"),("Sam","Mateo"),("Lucas","Pablo")] --Valida
l2 = [("Miguel","Marcos"),("Miriam","Julian"),("Marcos","Miguel"),("Marcos","Miguel")] --Invalida


--funcion aux para chequear si son iguales los elementos de una tupla 
sonIguales :: (String, String) -> Bool
sonIguales (a,b) | a == b = True
                 | otherwise = False

--aux para chequear si aparece en la lista
perteneceLista :: (String, String) -> [(String, String)] -> Bool
perteneceLista _ [] = False 
perteneceLista (a,b) ((c,d):xs) | a == c || a == d || b == c || b == d = True --si almenos un elemento aparece, devuelve true. Está bien porque el nombre no tiene que aparecer doble en ningun caso
                                |otherwise = perteneceLista (a,b) xs  --no está definido cuando no pertenece
 



 