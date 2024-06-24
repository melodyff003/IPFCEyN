{-
problema aproboMasDeNMaterias (registro: seq<seq<Char> x seq<Z>>,alumno:seq<Char>,n:Z):Bool{
    requiere:{No hay nombres de alumnos repetidos en registro}
    requiere:{Las notas de registro son todas mayores o iguales a cero y menores o iguales a 10}
    requiere:{n>0}
    requiere:{El alumno se encuentra en el registro}
    asegura:{Res=True <-> el alumno tiene mas de n notas de finales mayores o iguales a 4 en el registro}
}
--No hay nombres repetidos, y como alumno se encuentra en el registro, al menos hay un elemento cuyo nombre de alumno sea alumno
-}

registro :: [([Char], [Int])]
registro = [("Ester", [6,4,10]), ("Claudio", [10,10,10,10]), ("Juanito", [8,9,4]), ("Melody", [2, 10, 10 ,10])]


aproboMasDeNMaterias :: [([Char], [Int])] -> [Char] -> Int -> Bool
aproboMasDeNMaterias [_,_] [ ] _ = False 
aproboMasDeNMaterias ((alumno, finales):xs) nombre n | alumno == nombre = n < aprobadas finales
                                                     | otherwise = aproboMasDeNMaterias xs nombre n 

aprobadas :: [Int] -> Int
aprobadas [] = 0
aprobadas (f:fs) | f >= 4 = 1 + aprobadas fs 
                 | otherwise = aprobadas fs 


---ej 2 
{-
problema buenosAlumnos (registro: seq<seq<Char> x seq(Z)>>): seq<seq<Char>> {
    requiere: {No hay nombres de alumnos repetidos en registro}
    requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
    asegura: {res es la lista de los nombres de los alumnos que están en registro cuyo promedio 
    de notas es mayor o igual a 8 y no tiene aplazos osea, notas menores que 4}
Para resolver el promedio pueden utilizar la función del Preludio de Hasks fromIntegral que dado un valor de 
tipo Int devuelve su equivalente de tipo Float
-}
--fromIntegral :: Int -> Float

buenosAlumnos :: [([Char], [Int])] -> [[Char]]
buenosAlumnos [] = []
buenosAlumnos ((alumno, notas):xs) | promedio (notasTotales notas) (cantidadMaterias notas) >= 8 && not (aplazo notas) = [alumno] ++ buenosAlumnos xs 
                                   | otherwise = buenosAlumnos xs 

promedio :: Int -> Int -> Float  --suma todas las notas
promedio totalNotas materias = fromIntegral( totalNotas )/fromIntegral (materias)   

notasTotales :: [Int] -> Int
notasTotales [] = 0
notasTotales (x:xs) = x + notasTotales xs

cantidadMaterias :: [Int] -> Int 
cantidadMaterias [] = 0 
cantidadMaterias (x:xs) | x >= 0 = 1 + cantidadMaterias xs 
                        | otherwise = cantidadMaterias xs 

aplazo :: [Int] -> Bool
aplazo [] = False
aplazo (x:xs) | x < 4 = True 
              | otherwise = False 

--Ejercicio 3
{-
problema mejorPromedio (registro: seq<seq<Char> x seq(Z)>>): seq<Char> {
    requiere: {No hay nombres de alumnos repetidos en registro}
    requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
    requiere: {|registro|>0}
    asegura: {res es el nombre del alumno cuyo promedio de notas es el mas alto; si hay mas de un alumno con el mismo promedio de notas,devuelve el nombre de alumno que aparece primero en registro}
-}

mejorPromedio :: [([Char], [Int])] -> [Char]
mejorPromedio [(x, _)] = x 
mejorPromedio ((al,a):(al2,b):xs) | promedio (notasTotales a) (cantidadMaterias a) >= promedio (notasTotales b) (cantidadMaterias b) = mejorPromedio ((al,a):xs)
                                  | otherwise = mejorPromedio ((al2,b):xs)