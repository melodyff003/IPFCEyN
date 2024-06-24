
--este fue el parcial que me tomaron :| 

mapeo :: [(Char,Char)]
mapeo = [('a', 'c'), ('b','f'), ('g','l'), ('t', 'x')]
extraterrestre :: [Char]
extraterrestre = "extraterrestre"


----ej 1 
hayQueCodificar :: Char -> [(Char, Char)] -> Bool
hayQueCodificar _ [] = False 
hayQueCodificar c ((char,char1):xs) | c == char = True
                                    | otherwise = hayQueCodificar c xs 
----ej 2 
cuantasVecesHayQueCodificar :: Char -> [Char] -> [(Char,Char)] -> Int
cuantasVecesHayQueCodificar c frase mapeo | hayQueCodificar c mapeo = pertenece c frase
                                          | otherwise = 0 

pertenece :: Char -> [Char] -> Int --devuelve la cantidad de veces que aparece 
pertenece _ [] = 0
pertenece c (f:fs) | c == f = 1 + pertenece c fs
                   | otherwise = pertenece c fs 

----ej 3 
laQueMasHayQueCodificar :: [Char] -> [(Char, Char)] -> Char 
laQueMasHayQueCodificar [x] _ = x
laQueMasHayQueCodificar (f:fs) mapeo | cuantasVecesHayQueCodificar f (f:fs) mapeo >= cuantasVecesHayQueCodificar maximoLista (f:fs) mapeo = f
                                     | otherwise = maximoLista
                                     where maximoLista = laQueMasHayQueCodificar fs mapeo 

----ej 4
codificarFrase :: [Char] -> [(Char, Char)] -> [Char]
codificarFrase [] _ = []
codificarFrase (f:fs) mapeo | hayQueCodificar f mapeo = letraCodificada f mapeo: codificarFrase fs mapeo 
                            | otherwise = f: codificarFrase fs mapeo 


letraCodificada :: Char -> [(Char, Char)] -> Char -- para q me devuelva el segundo valor
letraCodificada _ [(_,b)] = b
letraCodificada x ((a,b):ys) | x == a = b
                             | otherwise = letraCodificada x ys 