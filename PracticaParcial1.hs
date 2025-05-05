
-- Ejercicio 1 (2 puntos) 
-- problema aproboMasDeNMaterias (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩, alumno:seq⟨Char⟩, n: Z) : Bool {
--   requiere: {No hay nombres de alumnos repetidos en registro}
--   requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
--   requiere: {n > 0}
--   requiere: {El alumno se encuentra en el registro }
--   asegura: {res = true <=> el alumno tiene más de n notas de finales mayores o iguales a 4 en el registro}
-- }

aproboMasDeNMaterias :: [(String,[Int])] -> String -> Int -> Bool
aproboMasDeNMaterias xs nombre num = (cantMateriasAprobadas alumno) > num
    where alumno = devolverAlumno xs nombre

devolverAlumno :: [(String,[Int])] ->  String -> (String,[Int])
devolverAlumno [] _ = ("",[])
devolverAlumno ((x,y):xs) n 
    | n == x = (x,y)
    |otherwise = devolverAlumno xs n

cantMateriasAprobadas :: (String,[Int]) -> Int
cantMateriasAprobadas (_,y) = cantMateriasAprobadasAux y

cantMateriasAprobadasAux :: [Int] -> Int
cantMateriasAprobadasAux [] = 0
cantMateriasAprobadasAux (x:xs)
    |x>=4 = 1 + cantMateriasAprobadasAux xs
    |otherwise = cantMateriasAprobadasAux xs

-- Ejercicio 2 (2 puntos)
-- problema buenosAlumnos (registro: seq⟨seq⟨Char⟩ x seq⟨Z⟩⟩) : seq⟨seq⟨Char⟩⟩ {
--   requiere: {No hay nombres de alumnos repetidos en registro}
--   requiere: {Las notas de registro son todas iguales o mayores a cero y menores o iguales a 10}
--   asegura: {res es la lista de los nombres de los alumnos que están en registro cuyo promedio de notas es mayor o igual a 8 y no tiene aplazos (notas menores que 4)}
-- }
-- Para resolver el promedio pueden utilizar la función del Preludio de Haskell fromIntegral que dado un valor de tipo Int devuelve su equivalente de tipo Float.

buenosAlumnos :: [(String,[Int])] -> [String]
buenosAlumnos [] = []
buenosAlumnos ((x,y):xs) 
    | promedioNotas y >=8 && tieneAplazos y == False = x:buenosAlumnos xs
    | otherwise = buenosAlumnos xs


tieneAplazos :: [Int] -> Bool
tieneAplazos [] = False
tieneAplazos (x:xs) 
    | x>=4 = tieneAplazos xs
    |otherwise = True
promedioNotas :: [Int] -> Float
promedioNotas [] = 0
promedioNotas xs = (fromIntegral (sumaNotas xs))/fromIntegral(longitud xs)

sumaNotas :: [Int] -> Int
sumaNotas [] = 0
sumaNotas (x:xs) = x + sumaNotas xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1+longitud xs

