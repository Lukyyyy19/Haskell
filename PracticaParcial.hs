-- EJERCICIO 1 (2 puntos)
-- problema mediaMovilN (lista: seq⟨Z⟩, n: Z) : Float {
--   requiere: {|lista| > 0}
--   requiere: {n > 0 ∧ n ≤ |lista|}
--   asegura: {res es el promedio de los últimos n elementos de lista}
-- }

mediaMovilN :: [Int] -> Int -> Float
mediaMovilN xs n = fromIntegral(sumaLista(ultimosNNumeros xs n)) / fromIntegral n

ultimosNNumeros :: [a] -> Int -> [a]
ultimosNNumeros [] _ = []
ultimosNNumeros [x] _ = [x]
ultimosNNumeros(x:xs) n 
    | longitud (x:xs) <= n = (x:xs)
    | otherwise = ultimosNNumeros xs n

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1+longitud xs

-- EJERCICIO 2 (2 puntos)    n>0
-- problema esAtractivo (n: Z) : Bool {
--   requiere: {n > 0}
--   asegura: {res = true <=> la cantidad de factores primos de n (distintos o no) es también un número primo.}
-- }
-- Aclaración: los factores primos de 30 son [5,3,2]. Los factores primos de 9 son [3,3]. 

esAtractivo :: Int -> Bool
esAtractivo x = esPrimo(longitud (separarLosPrimos(factoresDe x 1)))

separarLosPrimos :: [Int] -> [Int]
separarLosPrimos [] = []
separarLosPrimos (x:xs) 
    | esPrimo x = x:separarLosPrimos xs
    | otherwise = separarLosPrimos xs

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo x = minDivisor x 2 == x

minDivisor :: Int -> Int -> Int
minDivisor 0 _ = 0
minDivisor x n 
    |mod x n == 0 = n
    |otherwise = minDivisor x (n+1)

factoresDe :: Int -> Int -> [Int]
factoresDe x n 
    |x==n = [n]
    | mod x n == 0 = n:factoresDe x (n+1)
    | otherwise = factoresDe x (n+1)


-- EJERCICIO 3 (2 puntos)
-- problema palabraOrdenada (palabra: seq⟨Char⟩) : Bool {
--   requiere: {True}
--   asegura: {res = true <=> cada uno de los elementos no blancos de palabra es mayor o igual al anterior caracter no blanco, si existe alguno.}
-- }
-- Aclaración: 'a' < 'b' es True. 

palabraOrdenada :: [Char]->Bool
palabraOrdenada xs = estaOrdenada (eliminarBlancos xs)

estaOrdenada :: [Char] -> Bool
estaOrdenada (x:[y]) = x<=y
estaOrdenada (x:y:xs)
    | x<=y = estaOrdenada (y:xs)
    |otherwise = False

eliminarBlancos :: [Char] -> [Char]
eliminarBlancos [] = []
eliminarBlancos (x:xs)
    | x /= ' ' = x:eliminarBlancos xs
    |otherwise = eliminarBlancos xs


-- EJERCICIO 4 (3 puntos)
-- problema similAnagrama (palabra1: seq⟨Char⟩, palabra2: seq⟨Char⟩) : Bool⟩{
--   requiere: {True}
--   asegura: {res = true <=> (para todo caracter no blanco, la cantidad de apariciones de ese caracter en palabra1 es igual a la cantidad de apariciones en palabra2, 
-- y además existe al menos un caracter en palabra1 que tiene una posición distinta en palabra2)}
-- }

similAnagrama :: String -> String -> Bool
similAnagrama xs ys 
    | longitud xs /= longitud ys = False
    | otherwise=anagramaLetra xs xs ys


anagramaLetra :: [Char]-> [Char] -> [Char] -> Bool
anagramaLetra [] xs ys = True
anagramaLetra _ [x] [y] = x == y
anagramaLetra (k:ks) (xs) (ys) 
    | cuantasVecesSeRepite k (xs) == cuantasVecesSeRepite k (ys) = anagramaLetra ks xs ys
    | otherwise = False

cuantasVecesSeRepite :: Char -> String -> Int
cuantasVecesSeRepite _ [] = 0
cuantasVecesSeRepite c (x:xs)
    | c == x = 1 + cuantasVecesSeRepite c xsSinBlancos
    | otherwise = cuantasVecesSeRepite c xsSinBlancos
    where xsSinBlancos = eliminarBlancos xs