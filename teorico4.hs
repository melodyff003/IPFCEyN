--ejemplo de ejercicio recursivo 
esPar :: Int -> Bool 
esPar n | n == 0 = True
        | n == 1 = False 
        | otherwise= esPar(n - 2)

--arreglado recursivamente 
esPar2 :: Int -> Bool
esPar2 n | n == 0 = True
         | otherwise = not (esPar2(n - 1))

sumaLosPrimerosNImpares :: Integer -> Integer
sumaLosPrimerosNImpares n | n == 1 = 1
                          | n > 1 = n_esimoImpar + sumaLosPrimerosNImpares (n-1)
                          where n_esimoImpar = 2*n - 1
