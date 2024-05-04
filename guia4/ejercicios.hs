 --ej 1 
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0 
             | n == 1 = 1
             | n > 1 = fibonacci (n-1) + fibonacci (n-2)
             | otherwise = undefined


--ej 2 
parteEntera :: Float -> Integer
parteEntera n | n >= 0 && n < 1 = 0
              | n >= 1 = 1 + parteEntera(n - 1)
              | n >= -1 = -1
              | otherwise = (-1) + parteEntera(n + 1)

--ej 3
esDivisible :: Integer -> Integer -> Bool 
esDivisible x y | x == 0 = True
                | x < y = False
                | otherwise = esDivisible(x - y) y --para nros positivos

--ej 4
-- sumaImpares :: Integer -> Integer
-- sumaImpares n | n == 0 = 0
--               | even n = sumaImpares(n - 1)
--               | otherwise = n + sumaImpares(n - 1) + siguienteImpar n

-- siguienteImpar :: Integer -> Integer
-- siguienteImpar n | n == 0 = 0
--                  | even n = n + 1

sumaImpares2 :: Int -> Int
sumaImpares2 x| x == 0 = 0
             | otherwise = nEsimoImpar + sumaImpares2(x-1)
        where nEsimoImpar = (2*x)-1

--ej 5
medioFac :: Integer -> Integer --calcula el factorial cada 2
medioFac n | n <= 0 = 1
           | otherwise = n * medioFac(n - 2) 

--ej 6
