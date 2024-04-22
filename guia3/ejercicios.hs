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
maximo3 a b c | a > b &&  b > c = a
              | b > a && a > c = b 
              | c > a && c > b = c

--faltan casos para a > b y b < c 
maximo2 :: Int -> Int -> Int 
maximo2 a b | a > b = a
            |otherwise = b
