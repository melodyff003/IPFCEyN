productos :: [[Char]]
productos = ["yerba", "papa", "mate", "mesa", "agua", "termo", "papa", "papa", "queso", "agua", "agua", "agua", "agua", "agua", "agua" ,"agua" ,"agua" ,"agua" , "agua" ]

preciosProductos :: [([Char], Float)]
preciosProductos = [("yerba", 56.4), ("papa", 44), ("mate", 10.5), ("mesa", 5.5), ("agua", 200), ("termo", 40),("queso", 15)]

generarStock :: [[Char]] -> [([Char], Int)]
generarStock [] = []
generarStock (p:ps) = añadirProd p (generarStock ps)

-- hayProducto :: [Char] -> [[Char]] -> Int --cuenta las veces que aparece
-- hayProducto _ [] = 0
-- hayProducto producto (x:xs) | producto == x = 1 + hayProducto producto xs 
--                             | otherwise = hayProducto producto xs  

añadirProd :: [Char] -> [([Char], Int)] -> [([Char], Int)]
añadirProd p [] = [(p,1)]
añadirProd p ((n,c):ss) | p == n = ((n, c+1):ss) -- n = nombre prod, c = cantidad
 |otherwise = ((n,c): (añadirProd p ss))

 -----ejercicio 2
stockDeProducto :: [Char] -> [([Char], Int)] -> Int
stockDeProducto _ [] = 0
stockDeProducto producto ((nombre, cantidad):ps) | producto == nombre = cantidad
                                                 | otherwise = stockDeProducto producto ps 

---ejercicio 3 
dineroEnStock :: [([Char], Int)] -> [([Char], Float)] -> Float 
dineroEnStock [] _ = 0 
dineroEnStock ((p,c):xs) ps = (precioProd p ps) * fromIntegral( c ) + dineroEnStock xs ps


precioProd :: [Char] -> [([Char], Float)] -> Float 
precioProd prod ((n,v):ps) | prod == n = v 
                           | otherwise = precioProd prod ps 

---ejercicio 4
aplicarOferta :: [([Char], Int)] -> [([Char], Float)] -> [([Char], Float)]
aplicarOferta _ [] = []
aplicarOferta ((n,c):xs) ((prod, precio):ys) | stockDeProducto n xs > 5 = (prod, precio * 0.80 ): aplicarOferta xs ys
                                             | otherwise = ((prod,precio): aplicarOferta xs ys)
