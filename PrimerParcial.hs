module SolucionT2 where

--Nota: 10.0 / 10.0 (APROBADO)

-- puntaje ej1: 2
-- puntaje ej2: 2
-- puntaje ej3: 2
-- puntaje ej4: 2.5
-- puntaje ej5: 0.75
-- puntaje ej6: 0.75


--  Ejercicio 1 (2 puntos)

-- problema mayorSumaPosicionPares (n: Z, m: Z ) : Z {
--   requiere: {0 < n}
--   requiere: {0 < m}
--   asegura: {res = n si la suma de sus dígitos en posiciones pares es mayor o igual a la suma de los dígitos de las posiciones pares de m}
--   asegura: {res = m si la suma de sus dígitos en posiciones pares es mayor a la suma de los dígitos de las posiciones pares de n}
-- }

-- Aclaración: Las posiciones de los dígitos de un número se cuentan a partir de 1 y se corresponden con 1 para la unidad, 2 para las decenas, 3 para las centenas y así siguiendo.

--  Ejemplo: mayorSumaPosicionPares 143 2103 debe devolver 143
--  Ya que 4 ≥ (2+0)

-- Ejercicio 1
mayorSumaPosicionPares :: Integer -> Integer -> Integer
mayorSumaPosicionPares a 0 = a
mayorSumaPosicionPares n m 
    | sumarPosiciones n 1 >= sumarPosiciones m 1 = n
    |otherwise = m

sumarPosiciones :: Integer -> Integer -> Integer
sumarPosiciones 0 _ = 0
sumarPosiciones x v 
    |mod v 2 == 0 = mod x 10 + sumarPosiciones (div x 10) (v+1)
    |otherwise = 0 + sumarPosiciones (div x 10) (v+1)


--  Ejercicio 2 (2 puntos)
-- Un supermercado lleva registro del stock de mercaderías y le interesa saber cuándo debe reponer mercadería. Representaremos el stock del supermercado con una lista de tuplas compuesta por String x Z x Z, donde:

--     La primera componente de la tupla contiene el nombre del producto
--     La segunda componente de la tupla contiene el código del producto
--     La tercera componente de la tupla contiene la cantidad de producto que hay en la actualidad

-- Se pide implementar productosAReponer, que dado una lista de stock y una cantidad de producto que al menos debe haber en el stock actual, devuelva aquellos productos que deben ser respuestos con su nombre y código.

-- problema productosAReponer (s: seq⟨String x Z x Z⟩,cant: Z) :seq⟨String x Z⟩ {
--   requiere: { s[i]1 es algún producto de los que vende el supermercado para todo i tal que 0 ≤ i < |s|}
--   requiere: { no hay repetidos de producto s[i]1 }
--   requiere: { a cada producto s[i]1 solo le corresponde un único código s[i]2 }
--   requiere: { s[i]2 > 0 para todo i tal que 0 ≤ i < |s|}
--   requiere: { s[i]3 ≥ 0 para todo i tal que 0 ≤ i < |s|}
--   requiere: { cant > 0 }
--   asegura: { res contiene las tuplas de nombre y código de todos los productos incluídos en s tales que s[i]3 < cant }
--   asegura: { res contiene solamente las tuplas de nombre y código de todos los productos incluídos en s tales que s[i]3 < cant }
-- }

--  Ejemplo:  productosAReponer [("Fideos", 112, 20),("SalsaTomate", 203, 15),("Agua", 573, 4)] 5  debe devolver [("Agua", 573)]

-- Ejercicio 2     nombre prod  codigo   cantidad
productosAReponer :: [(String, Integer, Integer)] -> Integer -> [(String,Integer)]
productosAReponer [] _ = []
productosAReponer (x:xs) q 
    | hayQueReponer x q = producto:productosAReponer xs q
    |otherwise = productosAReponer xs q
    where producto = eliminarCantidad x
hayQueReponer :: (String,Integer,Integer) -> Integer -> Bool
hayQueReponer (x,y,z) q = z<q

eliminarCantidad ::(String,Integer,Integer) -> (String,Integer)
eliminarCantidad (x,y,z) = (x,y)

--  Ejercicio 3 (2 puntos)
-- En Haskell se puede representar una matriz utilizando una secuencia de secuencias, es decir, una secuencia donde cada elemento es a su vez una secuencia de igual longitud, representando las filas de la matriz. Por tanto, las columnas están formadas por los elementos en la misma posición de cada una de las filas.
-- Por ejemplo, en la matriz [[1,2],[3,4],[5,6]], la columna 1 está compuesta por los elementos en la posición 0 de cada fila: (1,3,5).

-- Se pide implementar la siguiente función:

-- problema hayColumnaSumaCero (matriz: seq⟨seq⟨Z⟩⟩) : Bool⟩{
--   requiere: {Todos los elementos de la secuencia matriz tienen la misma longitud}
--   requiere: {|matriz| > 0}
--   requiere: {|matriz[0]| > 0}
--   asegura: {res = true <=> existe j tal que 0 ≤ j < |matriz[0]| y la suma de los elementos matriz[i][j] es 0 para todo i con 0 ≤ i < |matriz|}
-- }

-- Aclaración: |matriz| es la longitud de la secuencia (representa la cantidad de filas) y |matriz[0]| es la longitud del primer elemento de la secuencia (representa la cantidad de columnas).

--  Ejemplo: hayColumnaSumaCero [[1,2],[0,-2]] debe devolver True
--  Ya que la columna 1 suma (1+0) y la columna 2 suma(2+(-2)), y, por tanto, existe al menos una columna cuya suma es cero.



-- Ejercicio 3
hayColumnaSumaCero :: [[Integer]] -> Bool
hayColumnaSumaCero [[1]] = False 
hayColumnaSumaCero [x] = contieneCero x
hayColumnaSumaCero mz = hayColumnaSumaCeroAux mz 0

hayColumnaSumaCeroAux ::[[Integer]] -> Integer ->Bool
hayColumnaSumaCeroAux [[]] _ = False
hayColumnaSumaCeroAux mz k 
    |sumarEnPosicionN mz k == 0 = True
    |sumarEnPosicionN mz k == 99 = False
    |otherwise = hayColumnaSumaCeroAux mz (k+1)

contieneCero :: [Integer] -> Bool
contieneCero [] = False
contieneCero [0] = True
contieneCero (x:xs) 
    |x == 0 = True
    |otherwise = contieneCero xs

cantidadDeColumnas :: [[Integer]] -> Integer
cantidadDeColumnas (x:xs) = cantidadDeColumnasAux x

cantidadDeColumnasAux ::[Integer] -> Integer
cantidadDeColumnasAux [] = 0
cantidadDeColumnasAux (x:xs) = 1 + cantidadDeColumnasAux xs

sumarEnPosicionN ::[[Integer]] -> Integer->Integer
sumarEnPosicionN [[]] _ = 0
sumarEnPosicionN [x] pos = devolverEnPosicion x pos
sumarEnPosicionN ([]:mz) pos = 99
sumarEnPosicionN ((x:xs):mz) pos = sumarEnPosicionNAux ((x:xs):mz) pos pos
    -- |devolverPosicion (x:xs) x == pos = x + sumarEnPosicionN mz pos
    -- |otherwise = sumarEnPosicionN (xs:mz) (pos-1)

sumarEnPosicionNAux :: [[Integer]] -> Integer -> Integer ->Integer
sumarEnPosicionNAux [[]] _ _ = 0
sumarEnPosicionNAux [x] pos p = devolverEnPosicion x pos
sumarEnPosicionNAux ([]:mz) pos p = 99
sumarEnPosicionNAux ((x:xs):mz) pos p
    |devolverPosicion (x:xs) x == pos = x + sumarEnPosicionNAux mz p p
    |otherwise = sumarEnPosicionNAux (xs:mz) (pos-1) p

devolverPosicion :: [Integer] -> Integer ->Integer
devolverPosicion [] _ = -99
devolverPosicion (x:xs) n
    |x==n = 0
    |otherwise = 1 + devolverPosicion xs n

devolverEnPosicion :: [Integer] -> Integer ->Integer
devolverEnPosicion [] _ = -99
devolverEnPosicion (x:xs) pos
    |devolverPosicion (x:xs) x == pos = x
    |otherwise = devolverEnPosicion (xs) (pos-1)




--      Ejercicio 4 (2,5 puntos)

-- problema primerosKNumerosAritmeticos (k: Z) : seq⟨Z⟩ {
--   requiere: { k ≥ 1}
--   asegura: { res es la secuencia de los primeros k números aritméticos}
--   asegura: {|res| = k}
--   asegura: {Para cualquier i en el rango 0 ≤ i < k-1, se cumple que res[i] < res[i+1]}
-- }

-- Aclaración: Un número n se llama aritmético si el promedio de todos sus divisores es un número entero. Por ejemplo 6 es un número aritmético porque el promedio de todos sus divisores (1+2+3+6)/4 = 3, es un número entero.

--  Ejemplo: primerosKNumerosAritmeticos 10  debe devolver [1,3,5,6,7,11,13,14,15,17]


-- Ejercicio 4
primerosKNumerosAritmeticos :: Integer -> [Integer]
primerosKNumerosAritmeticos 1 = [1]
primerosKNumerosAritmeticos k = primerosKNumerosAritmeticosAux k 1


primerosKNumerosAritmeticosAux :: Integer -> Integer -> [Integer]
primerosKNumerosAritmeticosAux 0 _ = []
primerosKNumerosAritmeticosAux k n 
    |esAritmetico n = n:primerosKNumerosAritmeticosAux (k-1) (n+1)
    |otherwise = primerosKNumerosAritmeticosAux k (n+1)

esAritmetico :: Integer -> Bool
esAritmetico n = mod sumaDivisores longitudDivisores == 0 
    where sumaDivisores = sumarLista (todosSusDivisores n)
          longitudDivisores = longitud (todosSusDivisores n)

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sumarLista :: [Integer] -> Integer
sumarLista [] = 0
sumarLista (x:xs) = x + sumarLista xs

todosSusDivisores :: Integer -> [Integer]
todosSusDivisores n = todosSusDivisoresAux n n

todosSusDivisoresAux :: Integer -> Integer ->[Integer]
todosSusDivisoresAux n 0 = []
todosSusDivisoresAux n div 
    |mod n div == 0 = div : todosSusDivisoresAux n (div-1)
    |otherwise = todosSusDivisoresAux n (div - 1) 