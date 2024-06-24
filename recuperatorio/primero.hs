l1 = [("Miguel","Marcos"),("Miriam","Julian"),("Sam","Mateo"),("Lucas","Pablo")] --Valida
l2 = [("Miguel","Miguel"),("Miriam","Julian"),("Marcos","Miguel"),("Marcos","Miguel")] --Invalida
l3 = [("Miguel","Marcos"),("Miriam","Julian"),("Marcos","Miguel"),("Marcos","Miguel")]
--2 
formulasInvalidas :: [(String, String)] -> Bool 
formulasInvalidas [] = True
formulasInvalidas (x:xs) | not(elementosDuplicados x) = not (aparecenLista x xs) && formulasInvalidas xs 
                         | otherwise = False 

elementosDuplicados :: (String, String) -> Bool
elementosDuplicados (a,b) | a == b = True
                          | otherwise = False

aparecenLista :: (String, String) -> [(String, String)] -> Bool 
aparecenLista _ [] = False
aparecenLista (a,b) ((c,d): xs) | a == c || a == d || b == c || b == d = True
                                | otherwise = False 

--------------------------------------------------
-- porcentajeDeVotosAfirmativos :: [(String, String)] -> [Int] -> Int  -> Float
