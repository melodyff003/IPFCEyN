f :: Integer -> Integer 
f x | x == 1 = 8 
    | x == 4 = 131 
    | x == 16 = 16 

g :: Integer -> Integer
g y |y == 8 = 16
    |y == 16 = 4
    |y == 131 = 1 

--Ejercicio 1 a b c 
composicionA :: Integer -> Integer 
composicionA a |a == 8 = f(g 8)
               |a == 131 = f(g 131)
               |a == 16 = f(g 16) 

composicionB :: Integer -> Integer 
composicionB b |b == 1 = g(f 1)
               |b == 4 = g(f 4)
               |b == 16 = g(f 16) 
-------------------------------------------------------
--Ejercicio 2 a
absoluto :: Int -> Int
absoluto x | x >= 0 = x 
                   | otherwise = -x 
-- b 
maximoAbs :: Int -> Int -> Int 
maximoAbs x y | absoluto x > absoluto y = x
              | absoluto x < absoluto y = absoluto y
              |otherwise = -1 
--c 
maximo3 :: Int -> Int -> Int -> Int 
maximo3 a b c | a > b &&  a > c = a
              | b > a && b > c = b 
              | c > a && c > b = c
--d
algunoEs0 :: Float -> Float -> Bool 
algunoEs0 0 _ = True
algunoEs0 _ 0 = True 
algunoEs0 _ _ = False

--e 
ambos0 :: Float -> Float -> Bool
ambos0 0 0 = True 
--f 
mismoIntervalo :: Float -> Float -> Bool  
mismoIntervalo x y | x <= 3 && y <= 3 = True
                   | (x> 3 && x<= 7) && (y>3 && y<=7 ) = True 
                   | x>7 && y>7 = True 
                   |otherwise = False 
--G 
sumaDistintos:: Int -> Int -> Int -> Int 
sumaDistintos a b c | a == b = a + c 
                    | a == c = a + b 
                    | b == c = b + a 
                    |otherwise = a + b + c 
--h
esMultiploDe :: Int -> Int -> Bool 
esMultiploDe x y | mod x y == 0 = True 
                 |otherwise = False 
--i 
digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

--j 
digitoDecenas :: Int -> Int 
digitoDecenas x = mod (div x 10) 10 

digitoDecenas2 :: Int -> Int 
digitoDecenas2 x = mod x 100 --esta mal 

--Ejercicio 3 
estanRelacionados :: Integer -> Integer -> Bool 
estanRelacionados x y | x == 0 && y == 0 = False 
                      | mod x y == 0 = True
                      | otherwise = False

--Ejercicio 4 a
prodInter :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) 
prodInter (ax, ay) (bx, by) = (ax*bx, ay*by)

--b 
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (ax, ay) (bx, by) | ax < bx && ay < by = True
                            |otherwise = False
--c 
distanciaPtos :: (Float, Float) -> (Float, Float) -> Float
distanciaPtos (a,b) (c,d) = sqrt(((c - a) **2) + ((d - b) **2)) 

--d
sumaTerna :: (Int, Int, Int) -> Int 
sumaTerna (a, b, c) = a + b + c 

--e 
sumaMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer 
sumaMultiplos (a, b, c) k | mod a k == 0 && mod b k == 0 && mod c k == 0 = a + b +c 
                          | mod a k == 0 && mod b k == 0 = a + b
                          | mod a k == 0 && mod c k == 0 = a + c
                          | mod b k == 0 && mod b k == 0 = b + c
                          | mod a k == 0 = a 
                          | mod b k == 0 = b 
                          | mod c k == 0 = c 
                          | otherwise = 0
--f 
posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (a, b, c) | mod a 2 == 0 = 0 
                       | mod b 2 == 0 = 1 
                       | mod c 2 == 0 = 2
                       | otherwise = 4 

--g 
crearPar :: a -> b -> (a, b)
crearPar a b = (a, b) --no toma chars

--h 
invertirPar :: (a, b) -> (b, a)
invertirPar (a, b) = (b, a)

--ejercicio 5
todosMenores :: (Integer, Integer, Integer) -> Bool 
todosMenores (a, b, c) | f5 a > g5 a && f5 b > g5 b && f5 c > g5 c = True  
                       | otherwise = False 

--funciones aux
f5 :: Integer -> Integer 
f5 n | n <= 7 = n*n
     | otherwise  = 2*n - 1 

g5 :: Integer -> Integer
g5 n | mod n 2 == 0 = div n 2
     | otherwise = 3*n + 1 
--ej 6 

bisiesto :: Integer -> Bool
bisiesto a | mod a 4 /= 0 || mod a 100 == 0 && mod a 400 /= 0 = False 
           | otherwise = True 

--ej 7
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float 
distanciaManhattan (a,b,c) (d,e,f) = abs((a-d) + (b - e) + (c-f))

--ej 8 
comparar :: Integer -> Integer -> Integer 
comparar a b | sumaUltimosDig a > sumaUltimosDig b = -1
             | sumaUltimosDig a < sumaUltimosDig b = 1
             | otherwise = 0
             where sumaUltimosDig x = mod x 10 + mod (div x 10) 10


