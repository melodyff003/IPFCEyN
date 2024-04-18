--Teorico 3 diapo 177 
-- cantidadSoluciones :: (int a, int b, int c)
-- cantidadSoluciones a b c | discriminante > 0 = 2 
   --                      | discriminante == 0 = 1
     --                    | otherwise = 0 
       --                  where discriminante = b^2 - 4*a*c

cantidadDeSoluciones :: (Num a1, Num a2, Ord a2) => a2 -> a2 -> a2 -> a1
cantidadDeSoluciones a b c | discriminante > 0 = 2 | discriminante == 0 = 1 | otherwise = 0 where discriminante = b^2- 4*a*c
