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
sumaDigitos :: Integer -> Integer
sumaDigitos n | n == 0 = 0 
              | otherwise = ultimoDigito + sumaDigitos(div n 10)
              where ultimoDigito = mod n 10 

--ej 8 
iesimoDigito :: Integer -> Integer -> Integer 
iesimoDigito n i = mod (div n 10^(cantDigitos n - i)) 10 --no se usa recursión

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              |otherwise = cantDigitos(div n 10) + 1 

--ej 9
esCapicua :: Integer -> Bool 
esCapicua n | n < 10 = True
            | otherwise = n == invertir n

invertir :: Integer -> Integer
invertir n | n == 0 = 0 
           | otherwise = ultimo * (10 ^ ((cantDigitos n) - 1 )) + invertir (div n 10)
           where ultimo = mod n 10

--ej 10 sumatoria de funciones
--a 
f1 :: Integer -> Integer
f1 0 = 1 
f1 n = 2^n + f1(n-1)

--b 
f2 :: Integer -> Integer -> Integer
f2 _ 0 = 1
f2 q i =  q^i + f2 q (i-1)

--c
--ej 11 
