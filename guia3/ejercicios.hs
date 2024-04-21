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
